(ns alexandria.core
  (:refer-clojure :exclude [read-string])
  (:require [clojure.edn :refer [read-string]]
            [datahike.api :refer [q pull db transact entity] :as d]
            (ring.middleware [defaults :refer [wrap-defaults site-defaults]]
                              [reload :refer [wrap-reload]])
            (ring.util [anti-forgery :refer [anti-forgery-field]]
                       [response :refer [redirect]]
                       [codec :refer [url-encode url-decode]])
            [ring.adapter.jetty :refer [run-jetty]]
            (compojure [core :refer [defroutes GET POST]]
                       [route :as route])
            (hiccup [page :refer [html5 include-css]]
                    [form :refer [form-to text-field text-area hidden-field submit-button select-options]]
                    [element :refer [link-to]])
            [cemerick.friend :as friend]
            (cemerick.friend [workflows :as workflows]
                             [credentials :as creds])
            [java-time :refer [local-date zone-id]]
            (alexandria [util :refer :all]
                        [market :refer [prices cost]]
                        [diff :refer [flower]])))

(defn schemon [ident type cardinality]
  {:db/ident ident
   :db/valueType type
   :db/cardinality cardinality})

(defn one [ident type]
  (schemon ident type :db.cardinality/one))

(defn many [ident type]
  (schemon ident type :db.cardinality/many))

(def schema [
             (assoc (one :name :db.type/string)
               :db/unique :db.unique/identity)
             (assoc (one :email :db.type/string)
               :db/unique :db.unique/value)
             (one :money :db.type/long)
             (one :owner :db.type/ref)
             (one :amount :db.type/long)
             (one :of :db.type/ref)
             (one :text :db.type/string)
             (one :title :db.type/string)
             (one :chosen :db.type/boolean)

             (one :password :db.type/string)
             (many :role :db.type/keyword)
             ])

(def cfg {:store {:backend :file :path "data"}})

(declare conn)

(defn store-schema []
  (prn (transact conn schema)))

(defn connect []
  (def conn (d/connect cfg)))

(defn setup []
  (d/create-database cfg)
  (connect)
  (store-schema))

(defn add-person [name email]
  (transact conn [{:name name :email email}]))

(defn lookup [db m]
  (q {:find '[?id .]
      :where (for [[k v] m]
               ['?id k v])}
     db))

(defn ensure [db m]
  (if-not (lookup db m) [m]))

(defn add-text [title text]
  (let [e {:title title :text text}]
    (lookup (:db-after (transact conn [[:db.fn/call ensure e]]))
            e)))

(defn persons []
  (q '[:find [?name ...]
       :where [_ :name ?name]]
     @conn))

(defn get-day [inst]
  (local-date inst (zone-id)))

(defn article-url [title]
  (str "/a/" (url-encode title)))

(defn shares-for-ids [db ids]
  (from-keys #(or (q '[:find (sum ?amount) .
                       :with ?owner
                       :in $ ?id
                       :where
                       [?pos :of ?id]
                       [?pos :amount ?amount]
                       [?pos :owner ?owner]]
                     db %)
                  0)
             ids))

(def active-id '[[(active ?id)
                  [?id :chosen true]]
                 [(active ?id)
                  [?pos :of ?id]
                  [?pos :amount ?amount]
                  [(> ?amount 0)]]])

(defn active-ids [title]
  (map first (q '[:find ?id
                  :in $ % ?title
                  :where
                  [?id :title ?title]
                  (active ?id)]
                @conn
                active-id
                title)))

(defn charge [tres ids name-lr roundf]
  (let [price (int (roundf (- (cost (shares-for-ids (:db-after tres) ids))
                              (cost (shares-for-ids (:db-before tres) ids)))))]
    (transact conn [[:db/add name-lr :money (- (:money (entity @conn name-lr) 0)
                                               price)]])))

(defn do-buy [name id]
  (let [name-lr [:name name]
        foo (get-or-create {:of id
                            :owner name-lr}
                           [:amount])
        bar (+merge foo {:amount 100})
        ids (conj (active-ids (get-title id)) id)
        tres (transact conn [bar])]
    (charge tres ids name-lr #(Math/ceil %))))

(defn add-text-post [{{:keys [title text]} :params :as req}]
  (let [id (add-text title text)]
    (do-buy (:current (friend/identity req)) id)
    (redirect (article-url title))))

(defn user-info [user]
  (let [name (:current user)
        name-lr [:name name]]
    [:span.user-info
     (str name " " (:money (entity @conn name-lr)))]))

(defmacro apage [req options & contents]
  (if-not (map? options)
    `(apage ~req {} ~options ~@contents)
    `(let [req# ~req]
       (html5
         [:head
          [:title ~(if-let [title (:title options)]
                     `(str ~title " - Alexandria")
                     "Alexandria")]
          (include-css "/style.css")]
         [:body
          [:div.header
           (link-to "/" [:h3 "Alexandria"])
           (if-let [user# (friend/identity req#)]
             [:div
              (user-info user#)
              " "
              (link-to "/logout" "logout")]
             [:div
              (link-to "/login" "login")
              " "
              (link-to "/register" "register")])]
          ~@contents]))))

(defn text-page [req title id]
  (let [user (friend/identity req)
        text (q '[:find ?text .
                  :in $ ?title ?id
                  :where
                  [?id :title ?title]
                  [?id :text ?text]]
                @conn title id)
        ids (active-ids title)]
    (apage req {:title title}
      (link-to (article-url title) [:h1 title])
      (for [oid (sort ids)]
        (list " "
              (if (= id oid)
                (str id)
                (link-to (str (article-url title) "/" oid) (str oid)))))
      [:div text]
      (if (friend/authorized? #{:writer} user)
        (form-to [:post "/add-text"]
          (anti-forgery-field)
          (hidden-field :title title)
          [:div "article" (text-area :text text)]
          (submit-button "send it"))))))

(defn petal-class [cc]
  (str "petal " (name cc)))

(defn article-page [req title]
  (let [user (friend/identity req)
        name-lr [:name (:current user)]
        ids (active-ids title)
        texts (from-keys #(:text (entity @conn %)) ids)
        colors (into {} (map vector
                             ids
                             [:red :blue :green :yellow]))
        amounts (shares-for-ids @conn ids)
        own-amounts (from-keys #(or (q '[:find ?amount .
                                         :in $ ?id ?owner
                                         :where
                                         [?pos :of ?id]
                                         [?pos :amount ?amount]
                                         [?pos :owner ?owner]]
                                       @conn % name-lr)
                                    0)
                               ids)
        prcs (prices amounts)]
    (apage req {:title title}
      [:h1 title]
      (for [id ids]
        (list " " (link-to {:class (petal-class (colors id))} (str (article-url title) "/" id) (str id))))
      [:div
       (for [chunk (flower texts)]
         (if (string? chunk)
           chunk
           (for [id ids
                 :let [petal (chunk id)]
                 :when petal]
             [:span {:class (petal-class (colors id))} petal])))]
      (for [id ids]
        [:div {:class (colors id)}
         (str (int (* 100 (prcs id))) " " (own-amounts id))
         (if (< 0 (own-amounts id 0))
           (form-to [:post "/sell"]
             (anti-forgery-field)
             (hidden-field :text-id id)
             (submit-button "sell")))
         (if (< 100 (:money (entity @conn name-lr) 0))
           (form-to [:post "/buy"]
             (anti-forgery-field)
             (hidden-field :text-id id)
             (submit-button "buy")))
         (if (friend/authorized? #{:admin} user)
           (form-to [:post "/settle"]
             (anti-forgery-field)
             (hidden-field :text-id id)
             (submit-button "settle")))]))))

(defn get-title [id]
  (:title (entity @conn id)))

(defn get-or-create [m p]
  (if-let [id (lookup @conn m)]
    (pull @conn (conj p :db/id) id)
    m))

(defn buy [{:keys [params] :as req}]
  (let [id (Long. (:text-id params))
        title (get-title id)]
    (do-buy (:current (friend/identity req)) id)
    (redirect (article-url title))))

(defn sell [{:keys [params] :as req}]
  (let [id (Long. (:text-id params))
        name-lr [:name (:current (friend/identity req))]
        e (q '[:find (pull ?pos [:db/id :amount]) .
               :in $ ?id ?owner
               :where
               [?pos :of ?id]
               [?pos :owner ?owner]]
             @conn id name-lr)
        title (get-title id)
        tres (transact conn [[:db/add (:db/id e) :amount (- (:amount e)
                                                            (min (:amount e) 100))]])]
    (charge tres (active-ids title) name-lr #(Math/floor %))
    (redirect (article-url title))))

(defn settle [{:keys [params] :as req}]
  (let [id (Long. (:text-id params))
        title (get-title id)
        ws (apply +merge (map (partial apply array-map) (q '[:find ?owner ?amount
                :in $ ?id
                :where
                [?pos :of ?id]
                [?pos :owner ?owner]
                [?pos :amount ?amount]]
              @conn id)))
        all (map first (q '[:find ?pos
                            :in $ ?title
                            :where
                            [?pos :of ?id]
                            [?id :title ?title]]
                          @conn title))
        others (map first (q '[:find ?oid
                               :in $ ?id
                               :where
                               [?id :title ?title]
                               [?oid :title ?title]
                               [?oid :chosen]
                               [(!= ?id ?oid)]]
                             @conn id))]
    (transact conn
              (vec (concat (for [[owner amount] ws
                                 :let [e (entity @conn owner)]]
                             [:db/add owner :money (+ amount (:money e) 0)])
                           (for [pos all]
                             [:db/retractEntity pos])
                           (for [article others]
                             [:db/retract article :chosen])
                           [[:db/add id :chosen true]])))
    (redirect (str "/a/" title))))

(defn index [req]
  (apage req
    (for [[title] (q '[:find ?title
                       :in $ %
                       :where
                       [?id :title ?title]
                       (active ?id)]
                     @conn
                     active-id)]
      (list " " (link-to (article-url title) title)))
    (if (friend/authorized? #{:writer} (friend/identity req))
      (form-to [:post "/add-text"]
        (anti-forgery-field)
        [:div "Title" (text-field :title)]
        [:div "article" (text-area  :text)]
        (submit-button "send it")))))

(defroutes writer-routes
  (POST "/add-text" [] add-text-post)
  (POST "/sell" [] sell)
  (POST "/buy" [] buy))

(defroutes admin-routes
  (POST "/settle" [] settle))

(defn login-form [req]
  (apage req {:title "Login"}
    [:h3 "Login"]
    (form-to [:post "/login"]
      (anti-forgery-field)
      [:div "Username" [:input {:type "text" :name "username"}]]
      [:div "Password" [:input {:type "password" :name "password"}]]
      [:div (submit-button "login")])))

(defn register-form [req]
  (apage req {:title "Register"}
    [:h3 "Register"]
    (form-to [:post "/register"]
      (anti-forgery-field)
      [:div "Username" [:input {:type "text" :name "username"}]]
      [:div "Password" [:input {:type "password" :name "password"}]]
      [:div (submit-button "register")])))

(defn add-user [name password]
  (transact conn [{:name name
                   :password (creds/hash-bcrypt password)
                   :role [:writer]
                   :money 500}]))

(defn register-post [{{:keys [username password]} :params :as req}]
  (if (entity @conn [:name username])
    ; tktk show error
    (redirect "/register")
    (do
      (add-user username password)
      (redirect "/login"))))

(defroutes user-routes
  (friend/wrap-authorize writer-routes #{:writer})
  (friend/wrap-authorize admin-routes #{:admin}))

(defn user-creds [name]
  (let [{:keys [name password role]} (entity @conn [:name name])]
    {:username name
     :password password
     :roles role}))

(defroutes app
  (GET "/a/:title" [title :as req] (article-page req (url-decode title)))
  (GET "/a/:title/:id" [title id :as req] (text-page req (url-decode title) (Long. id)))
  (GET "/login" [] login-form)
  (GET "/register" [] register-form)
  (POST "/register" [] register-post)
  (GET "/" [] index)
  (friend/logout (GET "/logout" [] (redirect "/")))
  (friend/authenticate user-routes
                      {:default-landing-uri "/"
                       :credential-fn (partial creds/bcrypt-credential-fn user-creds)
                       :workflows [(workflows/interactive-form)]})
  (route/resources "/")
  (route/not-found "not found"))

(def users (read-string (slurp "users.edn")))

(defn cred-fn [& args]
  (prn args))

(def wrapped-app (-> app
                     (wrap-defaults site-defaults)))

(def reloadable-app
  (wrap-reload #'wrapped-app))

(defn -main []
  (connect)
  (run-jetty reloadable-app {:port 3001 :join? false}))

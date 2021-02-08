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

             (one :is-admin :db.type/boolean)
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

(defn add-text [title text]
  (get-in (transact conn [{:db/id "text" :title title :text text}])
          [:tempids "text"]))

(defn persons []
  (q '[:find [?name ...]
       :where [_ :name ?name]]
     @conn))

(defn get-day [inst]
  (local-date inst (zone-id)))

(defn article-url [title]
  (str "/a/" (url-encode title)))

(defn add-text-post [{{:keys [title text]} :params}]
  (let [id (add-text title text)]
    (redirect (str (article-url title) "/" id))))

(defn user-info [user]
  (let [name (:current user)
        name-lr [:name name]]
    [:div.user-info
     (str name " " (:money (entity @conn name-lr)))]))

(defmacro apage [options & contents]
  (if-not (map? options)
    `(apage {} ~options ~@contents)
    `(html5
       [:head
        [:title ~(if-let [title (:title options)]
                   `(str ~title " - Alexandria")
                   "Alexandria")]
        (include-css "/style.css")]
       [:body
        [:div.header
         (link-to "/" [:h3 "Alexandria"])
         ~(if-let [user (:user options)]
            `(user-info ~user))]
        ~@contents])))

(defn text-page [req title id]
  (let [user (friend/identity req)
        text (q '[:find ?text .
                  :in $ ?title ?id
                  :where
                  [?id :title ?title]
                  [?id :text ?text]]
                @conn title id)
        ids (map first (q '[:find ?id
                 :in $ ?title
                 :where
                 [?id :title ?title]]
               @conn title))]
    (apage {:title title :user user}
      (link-to (article-url title) [:h1 title])
      (for [oid (sort ids)]
        (list " "
              (if (= id oid)
                (str id)
                (link-to (str (article-url title) "/" oid) (str oid)))))
      [:div text]
      (form-to [:post "/add-text"]
        (anti-forgery-field)
        (hidden-field :title title)
        [:div "article" (text-area :text text)]
        (submit-button "send it")))))

(defn petal-class [cc]
  (str "petal " (name cc)))

(defn shares-for-page [db title]
  (let [ids (map first (q '[:find ?id
                           :in $ ?title
                           :where
                           [?id :title ?title]]
                          db title))
        amounts (apply +merge (map (partial apply array-map)
                                   (q '[:find ?id ?amount
                                        :in $ ?title
                                        :where
                                        [?id :title ?title]
                                        [?pos :of ?id]
                                        [?pos :amount ?amount]]
                                      db title)))]
    (into {} (for [id ids]
               [id (get amounts id 0)]))))

(defn article-page [req title]
  (let [user (friend/identity req)
        name-lr [:name (:current user)]
        texts (into {} (q '[:find ?id ?text
                            :in $ ?title
                            :where
                            [?id :title ?title]
                            [?id :text ?text]]
                          @conn title))
        ids (map vector
                 (sort (keys texts))
                 [:red :blue :green :yellow])
        amounts (shares-for-page @conn title)
        own-amounts (into {} (q '[:find ?id ?amount
                              :in $ ?title ?owner
                              :where
                              [?id :title ?title]
                              [?pos :of ?id]
                              [?pos :amount ?amount]
                              [?pos :owner ?owner]]
                            @conn title name-lr))
        prcs (prices amounts)]
    (apage {:title title :user user}
      [:h1 title]
      (for [[id cc] ids]
        (list " " (link-to {:class (petal-class cc)} (str  (article-url title) "/" id) (str id))))
      [:div
       (for [chunk (flower texts)]
         (if (string? chunk)
           chunk
           (for [[id cc] ids
                 :let [petal (chunk id)]
                 :when petal]
             [:span {:class (petal-class cc)} petal])))]
      (for [[id cc] ids]
        [:div {:class cc}
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
         (if (friend/authorized? #{:admin} (friend/identity req))
           (form-to [:post "/settle"]
             (anti-forgery-field)
             (hidden-field :text-id id)
             (submit-button "settle")))]))))

(defn get-title [id]
  (:title (entity @conn id)))

(defn get-or-create [m p]
  (if-let [id (q {:find '[?id .]
                  :where (for [[k v] m]
                           ['?id k v])}
                 @conn)]
    (pull @conn (conj p :db/id) id)
    m))

(defn charge [tres title name-lr roundf]
  (let [price (int (roundf (- (cost (shares-for-page (:db-after tres) title))
                                 (cost (shares-for-page (:db-before tres) title)))))]
    (transact conn [[:db/add name-lr :money (- (:money (entity @conn name-lr) 0)
                                               price)]])))

(defn buy [{:keys [params] :as req}]
  (let [id (Long. (:text-id params))
        name-lr [:name (:current (friend/identity req))]
        foo (get-or-create {:of id
                            :owner name-lr}
                           [:amount])
        bar (+merge foo {:amount 100})
        title (get-title id)
        tres (transact conn [bar])]
    (charge tres title name-lr #(Math/ceil %))
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
    (charge tres title name-lr #(Math/floor %))
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
                               [(!= ?id ?oid)]]
                             @conn id))]
    (prn (concat (for [[owner amount] ws
                            :let [e (entity @conn owner)]]
                        [:db/add owner :money (+ amount (:money e) 0)])
                      (for [pos all]
                        [:db/retractEntity pos])
                      (for [article others]
                        [:db/retractEntity article])))
    (transact conn
              (vec (concat (for [[owner amount] ws
                            :let [e (entity @conn owner)]]
                        [:db/add owner :money (+ amount (:money e) 0)])
                      (for [pos all]
                        [:db/retractEntity pos])
                      (for [article others]
                        [:db/retractEntity article]))))
    (redirect (str "/a/" title))))


(defn index [req]
  (apage {:user (friend/identity req)}
    (for [[title] (q '[:find ?title
                       :where
                       [_ :title ?title]]
                     @conn)]
      (list " " (link-to (article-url title) title)))
    (form-to [:post "/add-text"]
      (anti-forgery-field)
      [:div "Title" (text-field :title)]
      [:div "article" (text-area  :text)]
      (submit-button "send it"))))

(defroutes writer-routes
  (POST "/add-text" [] add-text-post)
  (POST "/sell" [] sell)
  (POST "/buy" [] buy))

(defroutes admin-routes
  (POST "/settle" [] settle))

(defn login-form [req]
  (html5
    [:h3 "Login"]
    (form-to [:post "/login"]
      (anti-forgery-field)
      [:div "Username" [:input {:type "text" :name "username"}]]
      [:div "Password" [:input {:type "password" :name "password"}]]
      [:div (submit-button "login")])))

(defroutes app
  (GET "/login" [] login-form)
  (friend/wrap-authorize writer-routes #{:writer})
  (friend/wrap-authorize admin-routes #{:admin})
  (GET "/" [] index)
  (GET "/a/:title" [title :as req] (article-page req (url-decode title)))
  (GET "/a/:title/:id" [title id :as req] (text-page req (url-decode title) (Long. id)))
  (route/resources "/")
  (route/not-found "not found"))

(def users (read-string (slurp "users.edn")))

(defn cred-fn [& args]
  (prn args))

(def wrapped-app (-> app
                     (friend/authenticate
                      {:default-landing-uri "/"
                       :credential-fn (partial creds/bcrypt-credential-fn users)
                       :workflows [(workflows/interactive-form)]})
                     (wrap-defaults site-defaults)))

(def reloadable-app
  (wrap-reload #'wrapped-app))

(defn -main []
  (connect)
  (run-jetty reloadable-app {:port 3001 :join? false}))

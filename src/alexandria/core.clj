(ns alexandria.core
  (:refer-clojure :exclude [read-string])
  (:require (clojure [edn :refer [read-string]]
                     [string :refer [trim]])
            [datahike.api :refer [q pull transact entity] :as d]
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
                        [market :refer [prices charge charge-collateral]]
                        [diff :refer [diff2 to-patch combine-patch overlapping-patch apply-select patch-to-select]])))

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
             (assoc (many :support :db.type/ref)
               :db/isComponent true)
             (one :text :db.type/string)
             (assoc (one :title :db.type/string)
               :db/unique :db.unique/identity)
             (assoc (many :proposal :db.type/ref)
               :db/isComponent true)
             (many :overlaps :db.type/ref)

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
               (if (= \_ (first (name k)))
                 [v (keyword (subs (name k) 1)) '?id]
                 ['?id k v]))}
     db))

(defn ensure-ent [db m]
  (if-not (lookup db m) [m]))

(defn get-or-create2 [m]
  (lookup (:db-after (transact conn [[:db.fn/call ensure-ent m]]))
          m))

(def overlaps '[[(overlaps ?id ?oid)
                 [?id :overlaps ?oid]]
                [(overlaps ?id ?oid)
                 [?oid :overlaps ?id]]])

(defn ensure-title [db title]
  (if-not (q '[:find ?id .
               :in $ ?title
               :where
               [?id :title ?title]]
             db title)
    [{:title title :text ""}]))

(defn add-text [title text]
  (transact conn [[:db.fn/call ensure-title title]])
  (let [article (entity @conn [:title title])
        chosen (:text article)
        patch (to-patch (diff2 chosen text))
        id (get-or-create2 {:text text :_proposal (:db/id article)})
        oids (->> (filter #(and (not= id (:db/id %))
                                (let [foo (combine-patch patch
                                                         (to-patch (diff2 chosen
                                                                          (:text %))))
                                      bar (overlapping-patch foo)]
                                  bar))
                          (:proposal article))
                  (map :db/id))]
    (transact conn [{:db/id id :overlaps oids}])
    id))

(defn persons []
  (q '[:find [?name ...]
       :where [_ :name ?name]]
     @conn))

(defn get-day [inst]
  (local-date inst (zone-id)))

(defn article-url [title]
  (str "/a/" (url-encode title)))

(defn shares-for-ids [db ids]
  (assoc (from-keys #(or (q '[:find (sum ?amount) .
                       :with ?owner
                       :in $ ?id
                       :where
                       [?id :support ?pos]
                       [?pos :amount ?amount]
                       [?pos :owner ?owner]]
                     db %)
                  0)
             ids)
    :parent 0))

(defn own-shares-for-ids [db ids owner]
  (from-keys #(or (q '[:find ?amount .
                       :in $ ?id ?owner
                       :where
                       [?id :support ?pos]
                       [?pos :owner ?owner]
                       [?pos :amount ?amount]]
                     db % owner)
                  0)
             ids))

(defn overlapping-siblings [id]
  (q '[:find [?oid ...]
       :in $ % ?id
       :where
       (overlaps ?id ?oid)]
     @conn overlaps id))

(defn overlapping-cluster [id]
  (conj (overlapping-siblings id) id))

(defn not-overlapping-siblings [id]
  (q '[:find [?oid ...]
       :in $ % ?id
       :where
       (?parent :proposal ?id)
       (?parent :proposal ?oid)
       [(not= ?id ?oid)]
       (not (overlaps ?id ?oid))]
     @conn overlaps id))

(defn get-title [id]
  (:title (:_proposal (entity @conn id))))

(defn get-or-create [m p]
  (if-let [id (lookup @conn m)]
    (pull @conn (conj p :db/id) id)
    m))

(defn +money [db id amount]
  [[:db/add id :money (+ (:money (entity db id)) amount)]])

(defn retract-empty [db id]
  (let [article (entity db id)]
    (if (and (= (:text article) "")
             (not (seq (:proposal article))))
      [[:db/retractEntity id]])))

(defn unsettle [db id]
  (let [shorts
        (q '[:find ?owner ?amount
             :in $ ?id
             :where
             [?id :support ?pos]
             [?pos :owner ?owner]
             [?pos :amount ?amount]
             [(< ?amount 0)]]
           db id)]
    (vec (concat (for [[owner amount] shorts]
                   ; give back collateral
                   [:db.fn/call +money owner (- amount)])
                 [[:db/retractEntity id]
                  [:db.fn/call retract-empty (:db/id (:_proposal (entity db id)))]]))))

(defn unsettle-unsupported [db id]
  (if-not (q '[:find ?pos .
               :in $ ?id
               :where
               [?id :support ?pos]
               [?pos :amount ?amount]
               [(< 0 ?amount)]]
             db id)
    (unsettle db id)))

(defn do-trade [name id amount]
  (let [name-lr [:name name]
        foo (get-or-create {:_support id
                            :owner name-lr}
                           [:amount])
        bar (+merge foo {:amount amount})
        tres (transact conn [bar])
        ids (overlapping-cluster id)
        price (+ (int (Math/ceil (charge (shares-for-ids (:db-before tres) ids)
                                         (shares-for-ids (:db-after tres) ids))))
                 (charge-collateral (own-shares-for-ids (:db-before tres) ids name-lr)
                                    (own-shares-for-ids (:db-after tres) ids name-lr)))]
    (transact conn [[:db.fn/call +money name-lr (- price)]
                    [:db.fn/call unsettle-unsupported id]])))

(defn add-text-post [{{:keys [title text]} :params :as req}]
  (let [id (add-text title (trim text))]
    (do-trade (:current (friend/identity req)) id 100)
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


(defn petal-class [cc]
  (str "petal " (name cc)))

(defn article-page [req title]
  (let [user (friend/identity req)
        name-lr [:name (:current user)]
        article (entity @conn [:title title])
        article-text (:text article)
        children (:proposal article)
        amounts (shares-for-ids @conn (map :db/id children))
        own-amounts (own-shares-for-ids @conn (map :db/id children) name-lr)]
    (apage req {:title title}
      [:h1 title]
      [:div.text
       ; tktk show multi diff with all children
       article-text]
      [:table
      (for [child children
            :let [id (:db/id child)
                  prcs (prices (select-keys amounts (conj (overlapping-cluster id) :parent)))]]
        (list
         [:tr [:td.text {:colspan 9}
               (diff2 article-text (:text child))]]
         [:tr
          [:td (str (int (* 100 (prcs id))))]
          (if (friend/authorized? #{:writer} user)
            (list
             [:td
              (form-to [:post "/trade"]
                (anti-forgery-field)
                (hidden-field :text-id id)
                (hidden-field :amount 100)
                (submit-button {:disabled (not (or (< 100 (:money (entity @conn name-lr) 0))
                                                   (< (own-amounts id 0) 0)))}
                               "long"))]
             [:td
              (form-to [:post "/trade"]
                (anti-forgery-field)
                (hidden-field :text-id id)
                (hidden-field :amount -100)
                (submit-button {:disabled (not (or (< 100 (:money (entity @conn name-lr) 0))
                                                   (< 0 (own-amounts id 0))))}
                               "short"))]
             [:td (str (own-amounts id))]))
          (if (friend/authorized? #{:admin} user)
            [:td
             (form-to [:post "/settle"]
               (str id " "
                    (vec (overlapping-cluster id)) " "
                    (vec (not-overlapping-siblings id)))
               (anti-forgery-field)
               (hidden-field :text-id id)
               (submit-button "settle"))])]))]
      (if (friend/authorized? #{:writer} user)
        (form-to [:post "/add-text"]
          (anti-forgery-field)
          (hidden-field :title title)
          [:div "article" (text-area :text article-text)]
          (submit-button "send it"))))))

(defn trade [{:keys [params] :as req}]
  (let [id (Long. (:text-id params))
        amount (Long. (:amount params))
        title (get-title id)]
    (do-trade (:current (friend/identity req)) id amount)
    (redirect (article-url title))))

(defn settle [{:keys [params] :as req}]
  (let [id (Long. (:text-id params))
        title (get-title id)

        longs
        (q '[:find ?owner ?amount
             :in $ ?id
             :where
             [?id :support ?pos]
             [?pos :owner ?owner]
             [?pos :amount ?amount]
             [(< 0 ?amount)]]
           @conn id)

        neutral-texts
        (->> (not-overlapping-siblings id)
             (map (partial entity @conn)))

        old-text (:text (:_proposal (entity @conn id)))
        new-text (:text (entity @conn id))
        patch (to-patch (diff2 old-text new-text))

        transactions
        (concat (for [[owner amount] longs]
                  [:db.fn/call +money owner amount])
                (for [text neutral-texts
                      :let [patch2 (combine-patch patch
                                                  (to-patch (diff2 old-text
                                                                   (:text text))))
                            newtext (apply-select old-text
                                                  (patch-to-select patch2))]]
                  [:db/add (:db/id text) :text newtext])
                [[:db/retractEntity id]
                 {:title title :text new-text}]
                (for [sibling (overlapping-siblings id)]
                  [:db.fn/call unsettle sibling]))]
    (transact conn (vec transactions))
    (redirect (str "/a/" title))))

(defn index [req]
  (apage req
    (for [title (q '[:find [?title ...]
                     :where
                     [_ :title ?title]]
                   @conn)]
      (list " " (link-to (article-url title) title)))
    (if (friend/authorized? #{:writer} (friend/identity req))
      (form-to [:post "/add-text"]
        (anti-forgery-field)
        [:div "Title" (text-field :title)]
        [:div "article" (text-area  :text)]
        (submit-button "send it")))))

(defroutes writer-routes
  (POST "/add-text" [] add-text-post)
  (POST "/trade" [] trade))

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

(defn make-admin [name]
  (transact conn [{:name name
                   :role [:admin]}]))

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

(defn cred-fn [& args]
  (prn args))

(def wrapped-app (-> app
                     (wrap-defaults site-defaults)))

(def reloadable-app
  (wrap-reload #'wrapped-app))

(defn -main []
  (connect)
  (run-jetty reloadable-app {:port 3001 :join? false}))

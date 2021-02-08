(ns alexandria.util)

(def +merge (partial merge-with +))

(defn map-second [f xs]
  (map (fn [[a b]]
         [a (f b)])
       xs))

(defn map-map [f xs]
  (into {} (map-second f xs)))

(defn normalize-bag [m]
  (let [total (apply + (vals m))]
    (map-map #(/ % total) m)))


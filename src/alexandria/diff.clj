(ns alexandria.diff
  (:require [clojure.string :as string]
            [alexandria.util :refer :all]))

(defn prefix [xss]
  (let [limit (apply min (map count xss))]
    (loop [i 0]
      (if (and (< i limit)
               (apply = (map #(nth % i) xss)))
        (recur (inc i))
        i))))

(defn flower [yss]
  (let [xss (vals yss)
        pre (prefix xss)
        post (min (prefix (map string/reverse xss))
                  (- (apply min (map count xss)) pre))]
    [(subs (first xss) 0 pre)
     (map-map #(subs % pre (- (count %) post)) yss)
     (subs (first xss) (- (count (first xss)) post))]))


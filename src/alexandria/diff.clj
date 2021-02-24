(ns alexandria.diff
  (:require [clojure.string :as string]
            [diff-match-patch-clj.core :refer [diff as-hiccup]]
            [alexandria.util :refer :all]))

(defn diff2 [a b]
  (as-hiccup (diff a b)))

(defn to-patch [d]
  (loop [res []
         i 0
         [[x _ y] & xs] d]
    (if x
      (condp = x
        :span (recur res (+ i (count y)) xs)
        :ins (recur (conj res [i i y]) i xs)
        :del (recur (conj res [i (+ i (count y)) ""]) (+ i (count y)) xs))
      res)))

(defn vis-patch [p]
  (prn p)
  (loop [i 0
         [[p0 p1 _] & ps] p]
    (when p0
      (dotimes [_ (- p0 i)]
        (print "."))
      (dotimes [_ (- p1 p0)]
        (print "X"))
      (recur p1 ps)))
  (prn)
  (loop [i 0
         [[p0 p1 _] & ps] p]
    (when p0
      (dotimes [_ (- p1 i)]
        (print "."))
      (print "i")
      (recur (inc p1) ps)))
  (prn))

(defn overlapping-patch [p]
  (some (fn [[a b]]
          (> (second a) (first b)))
        (partition 2 1 p)))

(defn combine-patch [a b]
  (sort-by (fn [[x0 x1 _]] [x0 x1]) (concat a b)))

(defn patch-to-select [p]
  (assert (not (overlapping-patch p)))
  (loop [res []
         i 0
         [[p0 p1 p2] & ps] p]
    (if p0
      (recur (conj res [i p0 p2]) p1 ps)
      (conj res [i nil ""]))))

(defn apply-select [s sel]
  (apply str (mapcat (fn [[start end ins]]
                       [(subs s start (or end (count s)))
                        ins])
                     sel)))

(defn test-two-patch [a b c]
  (let [ab (to-patch (diff2 a b))
        ac (to-patch (diff2 a c))
        abc (combine-patch ab ac)]
    (prn ab)
    (prn ac)
    (prn abc)
    (apply-select a (patch-to-select abc))))

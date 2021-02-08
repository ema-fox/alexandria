(ns alexandria.market
  (:require [alexandria.util :refer :all]))

(def loss 100)

(defn exploss [x]
  (Math/exp (/ x loss)))

(defn prices [shares]
  (normalize-bag (map-map exploss shares)))

(defn cost [shares]
  (* loss (Math/log (apply + (map exploss (vals shares))))))

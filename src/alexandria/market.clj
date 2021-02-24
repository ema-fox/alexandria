(ns alexandria.market
  (:require [alexandria.util :refer :all]))

(def loss 100)

(defn exploss [x]
  (Math/exp (/ x loss)))

(defn prices [shares]
  (normalize-bag (map-map exploss shares)))

(defn cost [shares]
  (* loss (Math/log (apply + (map exploss (vals shares))))))

(defn collateral [shares]
  (- (apply + (map (partial min 0) (vals shares)))))

(defn charge [before after]
  ; (assert (is-subset (keys all-shares) (keys adjust-shares)))
  (- (cost after)
     (cost before)))

(defn charge-collateral [before after]
  (- (collateral after)
     (collateral before)))

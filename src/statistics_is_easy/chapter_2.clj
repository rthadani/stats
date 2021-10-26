(ns statistics-is-easy.chapter-2
  (:require [clojure.core.reducers :as r]))

(def bootstraps 10000)

(defn average
  [values]
  (/ (apply + values) (count values)))

(defn M>?
  [e values]
  (> (average values) e))

(defn power
  [values statistic-check power]
  (->> (range 1000)
       (r/reduce
        (fn [acc _] (if (statistic-check (repeatedly bootstraps #(rand-nth values))) (inc acc) acc))
        0)
       (< (* power 1000))))

#_(power (repeatedly 100 #(rand-int 1000)) (partial M>? 800) 0.9)

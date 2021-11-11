(ns statistics-is-easy.chapter-2
  (:require [clojure.core.reducers :as r]))

(def bootstraps 1000)

(defn avg
  [values]
  (/ (apply + values) (count values)))

(defn M>?
  [e values]
  (if (coll? e)
    (> (avg values) (avg (repeatedly (count values) #(rand-nth e))))
    (> (avg values) e)))

(defn power-experiment
  [values bootstraps statistic-check experiment-number]
  #_(when (zero? (mod experiment-number 10))
      (println "running experiment " experiment-number))
  (->> (range bootstraps)
       (r/reduce
        (fn [acc _] (if (statistic-check (repeatedly (count values) #(rand-nth values)))
                      (inc acc)
                      acc))
        0)
       (< (* 0.95 bootstraps))))

(defn power
  [values bootstraps statistic-check p]
  (->> (range 1000) ;;do the following 1000 times
       (pmap (partial power-experiment values bootstraps statistic-check))
       (filter true?) ;;filter out successes - where the null hypothesis is false
       count ;;count them
       (< (* p 1000)))) ;;See if it rejects the null hypothesis atleast power times number of tries

(def values (repeatedly 10 #(rand-int 10)))
(println values)
#_(power  values bootstraps (partial M>? 6) 0.8)

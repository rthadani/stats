(ns statistics-is-easy.chapter-2
  (:require [clojure.core.reducers :as r]))

(def bootstraps 10000)

(defn avg
  [values]
  (/ (r/reduce + 0 values) (count values)))

(defn M>?
  [e values]
  (if (coll? e)
    (> (avg values) (avg (repeatedly (count values) #(rand-nth e))))
    (> (avg values) e)))

(defn power-experiment
  [values bootstraps statistic-check experiment-number]
  (when (zero? (mod experiment-number 10))
    (println "running experiment " experiment-number))
  (let [count-good (->> (range bootstraps)
                        (r/reduce
                         (fn [acc _] (if (statistic-check (repeatedly (count values) #(rand-nth values)))
                                       (inc acc)
                                       acc))
                         0))]
    {:num-bootstraps bootstraps
     :count-good count-good
     :observed-probability (float (/ count-good bootstraps))}))

(defn is-significant?
  [{:keys [observed-probability]}]
  (<= observed-probability 0.05))

(defn power
  [experiment p]
  (->> (range 1000) ;;do the following 1000 times
       (pmap #(experiment %)) ;;run the experiment
       (filter is-significant?) ;;filter out successes - experiments where the null hypothesis is false
       count ;;count them
       (< (* p 1000)))) ;;See if it rejects the null hypothesis atleast power times number of tries

(def values (repeatedly 10 #(rand-int 10)))
(println values)
#_(power  (partial power-experiment values bootstraps (partial M>? 6)) 0.8)

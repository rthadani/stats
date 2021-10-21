(ns stats.statistics-is-easy.chapter-4
  (:require [scicloj.notespace.v4.api :as notespace]
            [fastmath.random :as rand]
            [kixi.stats.core :as kcore]
            [kixi.stats.test :as ktest]
            [clojure.core.reducers :as r]))

(comment
  (notespace/restart! {})
  (notespace/restart-events!)
  (notespace/stop!))

;;Chi square tests - Measure the deviation of observed data from the expectation or to test the independance of two 
;;group categories

;; Test if a roll die is fair - 6 possible outcomes Roll die 60 times and keep results in a frequency chart. Also record expected outcome for each category

(def base-table
  {:description (vec (range 1 7))
   :expected (vec (repeat 6 10))
   :observed [14 16 6 9 5 10]})

(defn update-with-differences
  [base-table]
  (->> base-table
       ((juxt :observed :expected))
       (apply map -)
       vec
       (assoc base-table :difference)))

(defn update-with-difference-squared
  [base-table-with-diff]
  (->> base-table-with-diff
       :difference
       (map #(* % %))
       (assoc base-table-with-diff :squared-difference)))

(defn update-with-chi-squared-column
  [base-table-with-squared-difference]
  (->> base-table-with-squared-difference
       ((juxt :squared-difference :expected))
       (apply map /)
       (assoc base-table-with-squared-difference :chi-squared)))

(defn chi-squared-sum
  [chi-squared-table]
  (->> chi-squared-table
       :chi-squared
       (apply +)))

(defn chi-squared
  [base-table observations]
  (->> (assoc base-table :observed observations)
       update-with-differences
       update-with-difference-squared
       update-with-chi-squared-column
       chi-squared-sum
       float))

#_(chi-squared base-table (:observed base-table))

(defn find-observation-bin
  [bin-maxes observation]
  (reduce
   (fn [bin bin-max] (if (> observation bin-max) (inc bin) (reduced bin)))
   0
   bin-maxes))

#_(find-observation-bin [10 20 30 40 50 60] 25)

(defn draw-from-categories
  [expected n]
  (let [num-bins (count expected)
        max-val (apply + expected)
        bins (reduce (fn [buckets size] (conj buckets (+ (or (last buckets) 0) size))) [] expected)
        observed (vec (repeat num-bins 0))]
    (r/reduce (fn [observed output]
                (update observed (find-observation-bin bins output) inc))
              observed
              (repeatedly n #(rand/irand 1 (inc max-val))))))

#_(chi-squared base-table (draw-from-categories (:expected base-table) 60))

(defn chi-squared-experiment
  [base-table bootstraps observations n]
  (let [observed-chi-squared (chi-squared base-table observations)
        expected (:expected base-table)]
    (r/reduce
     (fn [successes this-observation]
       (if (>= (chi-squared base-table this-observation)
               observed-chi-squared) (inc successes) successes))
     0
     (repeatedly bootstraps #(->> (draw-from-categories (:expected base-table) n))))))

(def successes (chi-squared-experiment base-table  10000 (:observed base-table) 60))
(def observed-chi-squared (chi-squared base-table (:observed base-table)))

(println (format "Observed chi-squared: %.2f" observed-chi-squared))
(println (format "%d out of 10000 experiments had a chi-squared difference greater than or equal to %.2f" successes observed-chi-squared))
(println (format "Probability that chance alone gave us a chi-squared greater than or equal to %.2f is %.2f" observed-chi-squared (float (/ successes 10000))))

#_(ktest/chi-squared-test)

(def wealth-table
  {:sick {:poor 20 :middle 18 :rich 8}
   :healthy {:poor 24 :middle 24 :rich 16}})

;;check if the chi square can be applied 
;;atleast 10 for each category
;;degrees of freedom >= 2  = (r - 1)* (c - 1)

(mapcat identity (vals wealth-table))

(defn table-with-row-totals
  [wealth-table]
  (reduce (fn [with-total [key row]]
            (assoc-in with-total [key :total] (apply + (vals row))))
          wealth-table
          wealth-table))
#_(wealth-table-with-totals wealth-table)
#_(->> wealth-table wealth-table-with-totals vals
       (mapcat #(dissoc % :total))
       (group-by first)
       vals
       (map #(apply + (map second %))))

(defn flatten-table
  [table-with-totals]
  (let [row-totals (map :total (vals table-with-totals))
        col-totals (->> table-with-totals
                        vals
                        (mapcat #(dissoc % :total))
                        (group-by first)
                        vals
                        (map #(apply + (map second %))))
        total-pop (apply + col-totals)
        expected (vec (for [rt row-totals ct col-totals] (float (* ct (/ rt total-pop)))))]
    (prn (apply + expected))
    (as-> (for [[key cols] table-with-totals
                [col val] cols
                :when (not= col :total)]
            [(str (name key) ":" (name col)) val]) $
      (reduce (fn [table [k v]]
                (-> table
                    (update :description conj k)
                    (update :observed conj v)))
              {:description [] :observed [] :expected []} $)
      (assoc $ :expected expected :row-totals (vec row-totals) :column-totals (vec col-totals)))))

(defn chi-shuffle
  [observed row-ttoal])

#_(let [flat-table (flatten-table (table-with-row-totals wealth-table))]
    (chi-squared flat-table (:observed flat-table)))

(ns statistics-is-easy.chapter-4-fishers
  (:require [clojure.core.reducers :as r]))

(def tea-table
  {:milk-first {:milk-first 3 :tea-first 1}
   :tea-first {:milk-first 2 :tea-first 4}})

(defn make-matrix
  [tea-table]
  (->> tea-table
       vals
       (reduce (fn [m r] (conj m (vec (vals r)))) [])))

(defn row-totals
  [table]
  (mapv #(apply + %) table))

(defn column-totals
  [table]
  (apply mapv (fn [& args] (apply + args)) table))

#_((juxt row-totals column-totals) (make-matrix tea-table))

(defn shuffle-mat
  "Create a matrix with random values while trying to keep the row and column marginals which are the results of the original observation
  Start by picking a random value for a non edge then adjust the others "
  [row-totals col-totals]
  (let [result-array (vec (repeat (count row-totals) (vec (repeat (count col-totals) 0))))
        num-cols (count col-totals)
        num-rows (count row-totals)]
    (loop [i 0
           j 0
           available-row-vals row-totals
           available-col-vals col-totals
           r result-array]
      (cond
        (= i num-rows) r
        (= j num-rows) (recur (inc i) 0 available-row-vals available-col-vals r)
        (and (= j (dec num-cols)) (= i (dec num-rows))) (recur i (inc j)
                                                               (update available-row-vals i - (available-col-vals j))
                                                               (update available-col-vals j - (available-col-vals j))
                                                               (assoc-in r [i j] (available-col-vals j)))

        (= j (dec num-cols)) (recur i (inc j)
                                    (update available-row-vals i - (available-row-vals i))
                                    (update available-col-vals j - (available-row-vals i))
                                    (assoc-in r [i j] (available-row-vals i)))
        (= i (dec num-rows)) (recur i (inc j)
                                    (update available-row-vals i - (available-col-vals j))
                                    (update available-col-vals j - (available-col-vals j))
                                    (assoc-in r [i j] (available-col-vals j)))

        :else (let [max-val (min (available-row-vals i) (available-col-vals j))
                    new-val (rand-int (inc max-val))]
                (recur i (inc j)
                       (update available-row-vals i - new-val)
                       (update available-col-vals j - new-val)
                       (assoc-in r [i j] new-val)))))))

(defn factorial
  [n]
  (loop [cnt n
         acc 1]
    (if (<= cnt 0)
      acc
      (recur (dec cnt) (* acc cnt)))))

(factorial 3)

(defn probability-matrix
  "Calculate the probability of the matrix using the n choose k formula for successes where the successes are the colomns in the table you observe. 
  In case of the tea/milk first it will be the ability to choose for example
  milk-first from milk-first/tea-first or a + b chhose a and a + c choose a"
  [m]
  (let [total-factorial (->> m (apply concat) (apply +) factorial)
        row-factorials (map #(factorial (apply + %)) m)
        column-factorials (apply map #(factorial (+ % %2)) m)
        individual-factorials (map #(factorial %) (flatten m))]
    (float (/ (* (apply * row-factorials) (apply * column-factorials)) (* total-factorial (apply * individual-factorials))))))

(defn fischers-exact-test
  "The one tailed test for the probability of the f test.
  Set vakues for a fill up the other parts of the matrix based on the marginal values.
  Calculate the probability by adding up the values for every variation where the success is greater than the observed success (a + d)
  where a is milk is used first and identified and d is tea observed first and identified"
  [matrix]
  ;;assumes a 2x2 matrix 
  (let [[a b c d :as m] (flatten matrix)
        total (apply + m)]
    (r/reduce (fn [prob-tail a-prime]
                (let [m [[0 0] [0 0]]
                      b-prime (- (+ a b) a-prime)
                      c-prime (- (+ a c) a-prime)
                      d-prime (- (+ c d) c-prime)
                      m (cond-> (assoc-in m [0 0] a-prime)
                          true (assoc-in [0 1] b-prime)
                          (nat-int? b-prime) (assoc-in [1 0] c-prime)
                          (nat-int? c-prime) (assoc-in [1 1] d-prime))]
                  (if (and (every? nat-int? (flatten m)) (>= (+ a-prime d-prime) (+ a d)))
                    (+ prob-tail (probability-matrix m))
                    prob-tail)))
              0
              (range total))))

#_(fischers-exact-test (make-matrix tea-table) true)

(defn experiment
  [matrix bootstraps]
  (let [row-totals (row-totals matrix)
        col-totals (column-totals matrix)
        observed-prob (fischers-exact-test matrix)
        experimental-probs (repeatedly bootstraps #(fischers-exact-test (shuffle-mat row-totals col-totals)))
        successes (keep #(when (< % observed-prob) %) experimental-probs)]
    {:num-bootstraps bootstraps
     :count-good (count successes)
     :observed-probability (float (/ (count successes) bootstraps))}))

#_(experiment (make-matrix tea-table) 10000)
#_(prob-matrix (shuffle-mat [4 6] [5 5]))
#_(prob-matrix [[3 1] [2 4]])

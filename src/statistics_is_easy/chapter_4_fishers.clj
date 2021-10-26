(ns statistics-is-easy.chapter-4-fishers)

(def tea-table
  {:milk-first {:milk-first 3 :tea-first 1}
   :tea-first {:milk-first 2 :tea-first 4}})

(defn add-row-totals
  [table]
  (let [row-totals (mapv #(apply + %) table)]
    (mapv #(conj %1 %2) table row-totals)))

(defn add-column-totals
  [table]
  (let [col-totals (apply mapv (fn [& args] (apply + args)) table)]
    (conj table col-totals)))

(defn make-matrix
  [tea-table]
  (->> tea-table
       vals
       (reduce (fn [m r] (conj m (vec (vals r)))) [])
       add-row-totals
       add-column-totals))

(defn shuffle-mat
  [row-totals col-totals]
  (let [result-array (vec (repeat (count row-totals) (vec (repeat (count col-totals) 0))))]
    (loop [i 0
           j 0
           r result-array
           rt row-totals
           ct col-totals]
      (cond
        (and (= i (count rt)) (= j (count ct))) r
        (= j (count col-totals)) (recur (inc i) 0 r rt ct)
        ()
        :else (let [])))

    result-array))

(shuffle-mat [4 6] [5 5])

(ns stats.statistics-is-easy.chapter-2)

(def bootstraps 1000)

(defn avg
  [values]
  (/ (apply + values) (count values)))

(defn M>?
  [e values]
  (> (avg values) e))

(defn power
  [values statistic-check power]
  (-> (reduce
       (fn [acc _] (if (statistic-check (repeatedly bootstraps #(rand-nth values))) (inc acc) acc))
       (range 1000))
      (> (* power 1000))))

#_(power (repeatedly 100 #(rand-int 1000)) (partial M>? 800) 0.9)

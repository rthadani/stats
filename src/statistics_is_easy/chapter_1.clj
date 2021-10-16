(ns stats.statistics-is-easy.chapter-1
  (:require [aerial.hanami.templates :as ht]
            [kixi.stats.distribution :as kstatsd]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind] ; a collection of known kinds of notes
            [scicloj.kindly.api :as kindly]
            [scicloj.viz.api :as viz]))

(comment
  (notespace/restart! {})
  (notespace/restart-events!)
  (notespace/stop!))

(defn make-bar-chart-data
  [sample]
  (->> sample
       (group-by identity)
       (map (fn [[num-heads values]] {:x num-heads :y (count values)}))))

(-> (kstatsd/sample 10000 (kstatsd/binomial {:n 17 :p 0.5}))
    make-bar-chart-data
    viz/data
    (viz/x :x {:XTITLE "number of heads out of 17 tosses"})
    (viz/y :y {:YTITLE "number of samples"})
    (viz/type ht/bar-chart)
    viz/viz
    (kindly/consider kind/vega))

;;Do an experiment where we see 15 out of 17 heads in a fair coin
(defn apply-prob
  [p n]
  (let [draw-space (-> (/ 1 p) (* 1000) (+ 0.05))]
    (reduce (fn [success draw]
              (if (>= (* p draw-space)  draw)
                (inc success)
                success))
            0
            (repeatedly n #(rand-int draw-space)))))

(defn experiment
  [observed trials success-probability num-bootstraps]
  (let [experimental-successes (repeatedly num-bootstraps #(apply-prob success-probability trials))
        count-good (count (filter #(>= % observed) experimental-successes))]
    {:count-good count-good
     :num-bootstraps num-bootstraps
     :observed-probability (float (/ count-good num-bootstraps))}))

(defn k-stats-experiment
  [observed num-experiments success-probability num-bootstraps]
  (let [count-good (->> {:n num-experiments :p success-probability}
                        kstatsd/binomial
                        (kstatsd/sample num-bootstraps)
                        (filter #(>= % observed))
                        count)]
    {:count-good count-good
     :num-bootstraps num-bootstraps
     :observed-probability (float (/ count-good num-bootstraps))}))

(defn print-results [{:keys [count-good num-bootstraps observed-probability]} label]
  (println (format "%s -- %d out of %d times we got atleast the %d number of heads in %d tosses\nProbability that chance alone gave us atleast %d heads in %d tosses is %f" label count-good num-bootstraps 15 17 15 17 observed-probability)))

(print-results (k-stats-experiment 15 17 0.5 10000) "kixi-stats")
(print-results (experiment 15 17 0.5 10000) "sie-stats")

;;t-test to check the effectiveness of a drug
(def placebo [54 51 58 44 55 52 42 47 58 46])
(def drug [54 73 53 70 73 68 52 65 65])
(defn make-labels
  [sample label]
  (map (fn [s] [s label]) sample))

(defn avg
  [s]
  (-> (apply + s)
      (/ (count s))))

(defn avg-diff
  [s1 s2]
  (Math/ceil (- (avg s1) (avg s2))))

(defn shuffled-avg-diff
  [l1 l2]
  (let [all-together (concat l1 l2)
        label-real (second (first l1))
        label-fake (second (first l2))
        shuffled-labels (shuffle (map second all-together))
        new-labelled-1 (map (fn [s l] [s l]) (map first l1) shuffled-labels)
        new-labelled-2 (map (fn [s l] [s l]) (map first l2) (drop (count l1) shuffled-labels))
        [new-sample-1 new-sample-2] (->> (concat new-labelled-2 new-labelled-1)
                                         (group-by second)
                                         vals)
        [new-real-sample new-fake-sample] (if (= (second (first new-sample-1)) label-real)
                                            [new-sample-1 new-sample-2]
                                            [new-sample-2 new-sample-1])]
    (avg-diff (map first new-real-sample) (map first new-fake-sample))))

(def labelled-drug-sample (make-labels drug "D"))
(def labelled-placebo-sample (make-labels placebo "P"))

(->  (repeatedly 10000 #(shuffled-avg-diff labelled-drug-sample labelled-placebo-sample))
     make-bar-chart-data
     viz/data
     (viz/x :x {:XTITLE "difference between means"})
     (viz/y :y {:YTITLE "bootstrap samples"})
     (viz/type ht/bar-chart)
     viz/viz
     (kindly/consider kind/vega))

(let [observed-diff (avg-diff drug placebo)
      experiment-diff (repeatedly 10000 #(shuffled-avg-diff labelled-drug-sample labelled-placebo-sample))
      count-good (->> experiment-diff
                      (filter #(>= % observed-diff))
                      count)
      observed-probability (float (/ count-good 10000))]
  (println (format "%d out of %d experiments had a difference of two means greater than or equal to %f\nThe chance of getting a difference of two means greater than or equal to %f is %f" count-good 10000 observed-diff observed-diff observed-probability)))

;;(kstatsd/sample 100 (kstatsd/t {:v (dec (count drug))}))

;;confidence intervals for differennce in means
(defn bootstrap-avg
  [sample]
  (-> (repeatedly (count sample) #(rand-nth sample))
      avg))
#_(repeatedly (count drug) #(rand-nth drug))
#_(bootstrap-avg drug)

(defn confidence-intervals
  [percentile num-bootstraps drug placebo]
  (let [edge (/ (- 1 percentile) 2)
        lower-index (Math/ceil (* edge num-bootstraps))
        upper-index (Math/floor (* (- 1 edge) num-bootstraps))
        diffs (-> (repeatedly num-bootstraps #(- (bootstrap-avg drug) (bootstrap-avg placebo)))
                  sort)]
    {:lower (float (nth diffs lower-index))
     :upper (float (nth diffs upper-index))
     :min (first diffs)
     :max (last diffs)}))

#_(confidence-intervals 0.90 10000 drug placebo)

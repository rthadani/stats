(ns statistics-is-easy.chapter-1
  (:require [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [fastmath.stats :as fstat]
            [kixi.stats.core :as kcore]
            [kixi.stats.distribution :as kstatsd]
            [kixi.stats.test :as ktest]
            [nextjournal.clerk :as clerk]
            [scicloj.viz.api :as viz]))


;; ## Do an experiment where we see 15 out of 17 heads in a fair coin
;; What do successes(tossing a head) look like when doing it 17 times using a binomial distribution

(defn render-one
  [color x-title y-title xy]
  (hc/xform ht/bar-chart
                :DATA xy 
                :X :x
                :Y :y
                :XTITLE x-title
                :YTITLE y-title
                :YAGG "sum"
                :MCOLOR color
                :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}]))

(->> (kstatsd/sample 10000 (kstatsd/binomial {:n 17 :p 0.5}))
     (map (fn [s] {:x s :y 1}))
     (render-one "blue" "number of heads out of 17 tossses" "number of samples")
     clerk/vl)

(defn apply-prob
  "Create a large number of buckets to draw from  and split them up based on the p parameter.
 Any random drawing that draws from the right side is considered a success
 p indicates the probability of a success and is used to split a bucket into two
 n is the number of times we want to try an experiment in the case of this one we want to toss a coin 17 times"
  [p n]
  (let [draw-space (-> (/ 1 p) (* 1000) #_(+ 0.05))]
    (reduce (fn [success draw]
              (if (>= (* p draw-space)  draw)
                (inc success)
                success))
            0
            (repeatedly n #(rand-int draw-space)))))

(->> (repeatedly 10000 #(apply-prob 0.5 17))
     (map (fn [s] {:x s :y 1}))
     (render-one "blue" "number of samples" "number of heads out of 17 tossses")
     clerk/vl)


;; See what one run looks like when flipping an experiment 17 times
(apply-prob 0.5 17)
(defn render-ab
  [a-prob b-prob trials y-label]
  (-> (hc/xform ht/layer-chart
                :LAYER [(render-one "blue" "A" y-label (map (fn [s] {:x s :y 1}) (repeatedly 10000 #(apply-prob a-prob trials)))) 
                        (render-one "red" "B" "" (map (fn [s] {:x s :y 1}) (repeatedly 10000 #(apply-prob b-prob trials))))])
      clerk/vl))

(render-ab 0.5 (/ 15 17) 17 "number of samples")
(defn experiment
  [observed trials success-probability num-bootstraps]
  (let [experimental-successes (repeatedly num-bootstraps #(apply-prob success-probability trials))
        count-good (count (filter #(>= % observed) experimental-successes))]
    {:count-good count-good
     :num-bootstraps num-bootstraps
     :observed-probability (float (/ count-good num-bootstraps))}))

;; Do the experiment 10000 times of flipping a coin 17 times and noting whether a head or tails shows up.
;; If the probability(p-value) of the observed is small < 0.05 we reject the null hypothesis"
(experiment 15 17 0.5 10000)

; Do the same by drawing samples from  the binomial distribution in kixi stats 
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

(k-stats-experiment 15 17 0.5 10000)

(def population 17)
;;Try using the kixi stats library to determine the p-value
(ktest/p-value (ktest/simple-z-test {:mu (* population 0.5) :sd (Math/sqrt (* population 0.5 0.5))}
                                    {:mean 15 :n 17}))

(defn format-result
  "Print the result similar to whats in the book"
  [{:keys [count-good num-bootstraps observed-probability]} label]
  (format "%s -- %d out of %d times we got atleast the %d number of heads in %d tosses\nProbability that chance alone gave us atleast %d heads in %d tosses is %f" label count-good num-bootstraps 15 17 15 17 observed-probability))

(format-result (k-stats-experiment 15 17 0.5 10000) "kixi-stats")
(format-result (experiment 15 17 0.5 10000) "sie-stats")

;; ### Test to check the effectiveness of a drug
;; To check the effectiveness of a drug we test it against a placebo, the numbers indicate the measured improvement
(def placebo [54 51 58 44 55 52 42 47 58 46])
(def drug [54 73 53 70 73 68 52 65 65])

(defn avg
  "The average function should have used a library"
  [s]
  (-> (apply + s)
      (/ (count s))))

(defn avg-diff
  "Take the average and get a diff"
  [s1 s2]
  (Math/ceil (- (avg s1) (avg s2))))
;; 
(delay (avg-diff drug placebo))

(defn shuffled-avg-diff
  "Combine, shuffle, resplit the sample and the population and then check the differences in average. 
  If the difference turns out to be a by chance then other combinations of the values can also provide the 
  same difference in average as the observed value"
  [sample population]
  (let [all-together (shuffle (concat sample population))
        new-sample (take (count sample) all-together)
        new-population (drop (count sample) all-together)]
    (avg-diff new-sample new-population)))

;; ### Visually check the difference in means
(->> (repeatedly 10000 #(shuffled-avg-diff drug placebo))
     (map (fn [s] {:x s :y 1}))
     (render-one "blue" "difference between means" "bootstrap samples")
     clerk/vl)

;; ### Confidence intervals for difference in means
(defn bootstrap-avg
  [sample]
  (-> (repeatedly (count sample) #(rand-nth sample))
      avg))
(count drug)
(bootstrap-avg drug)

(defn drug-experiment
  [num-bootstraps drug placebo]
  (let [observed-diff (avg-diff drug placebo)
        experiment-diff (repeatedly num-bootstraps #(shuffled-avg-diff drug placebo))
        count-good (->> experiment-diff
                        (filter #(>= % observed-diff))
                        count)
        observed-probability (float (/ count-good num-bootstraps))]
    (println (format "%d out of %d experiments had a difference of two means greater than or equal to %f\nThe chance of getting a difference of two means greater than or equal to %f is %f" count-good num-bootstraps observed-diff observed-diff observed-probability))
    {:num-bootstraps num-bootstraps
     :count-good count-good
     :observed-probability observed-probability}))


(defn confidence-intervals
  "Pick a random sample with replacement calculate the statistic (difference in avgs)"
  [percentile num-bootstraps drug placebo tails]
  (let [edge (/ (- 1 percentile) tails)
        lower-index (Math/ceil (* edge num-bootstraps))
        upper-index (Math/floor (* (- 1 edge) num-bootstraps))
        diffs (-> (repeatedly num-bootstraps #(- (bootstrap-avg drug) (bootstrap-avg placebo)))
                  sort
                  vec)]
    {:lower (float (nth diffs lower-index))
     :upper (float (nth diffs upper-index))
     :min (float (first diffs))
     :max (float (last diffs))
     :avg (float (avg diffs))
     #_:raw-data #_diffs}))

;; 90% Confidence interval for drug and placebo 
(confidence-intervals 0.90 10000 drug placebo 2)

;;power
(defn is-significant?
  [{:keys [observed-probability]}]
  (<= observed-probability 0.05))

(defn power
  [experiment p]
  (->> (range 1000) ;;do the following 1000 times
       (pmap #(experiment %)) ;;run the experiment
       (filter is-significant?) ;;filter out successes - experiments where the null hypothesis is false
       count ;;count them
       #_(< (* p 1000)))) ;;See if it rejects the null hypothesis atleast power times number of tries

(delay (power (fn [experiment-number] (when (zero? (mod experiment-number 100))
                                        (println "Running experiment" experiment-number))
                (drug-experiment 10000 drug placebo)) 0.8))

(def xrule-chart
  (-> (assoc-in ht/xrule-layer [:encoding :x2] {:field :X2})
      (assoc :data ht/data-options)))

(let [ci (confidence-intervals 0.90 10000 drug placebo 2)]
  (-> (hc/xform ht/layer-chart
                :LAYER [(hc/xform xrule-chart
                                  :X "min"
                                  :XTYPE "quantitative"
                                  :X2 "max"
                                  :VALDATA [ci]
                                  :XZERO false)
                        (-> (hc/xform ht/bar-chart
                                      :X "lower"
                                      :YBIN hc/RMV
                                      :VALDATA [ci]
                                      :TOOLTIP hc/RMV
                                      :SIZE 14)
                            (update :encoding dissoc :y)
                            (assoc-in [:encoding :x2] {:field "upper"}))
                        (-> (hc/xform ht/point-chart
                                      :X "avg"
                                      :YBIN hc/RMV
                                      :VALDATA [ci]
                                      :SIZE 14
                                      :MCOLOR "red"
                                      :TOOLTIP hc/RMV)
                            (update :encoding dissoc :y)
                            (assoc :tooltip [{:field "avg"}]))]
                :HEIGHT 40)
      (clerk/vl)))

(delay (drug-experiment 10000 drug placebo))
(def placebo-measure-2 [56 348 162 420 440 250 389 476 288 456])
(def drug-measure-2 (mapv #(+ % 13) placebo-measure-2))
(delay (drug-experiment 10000 drug-measure-2 placebo-measure-2))

(-> (fstat/ttest-two-samples drug placebo)
    :p-value)
;; 0.001801742370493504

;;;; Kixi.stats using transducers ;;;;

;;;; Option A: calculate summary statistics independently ;;;;
;;
;; (We're using t-test, not simple-t-test because the latter compares a sample
;; against a population whereas we have two samples to compare

;; First define reducing function (rf) which returns summary statistics:

(def summary-stats
  {:mean kcore/mean
   :sd kcore/standard-deviation
   :n kcore/count})

(def summary-stats-rf
  (redux.core/fuse summary-stats))

;; Then calculate the pair of summaries and pass to t-test:

(-> (ktest/t-test (transduce identity summary-stats-rf placebo)
                  (transduce identity summary-stats-rf drug))
    (ktest/p-value))
;; 0.0018017423704935023

;;;; Option B: build a more sophisticated reducing function ;;;;

;; First reshape our data into a single labelled sequence:

(def data
  (concat (map (partial hash-map :placebo) placebo)
          (map (partial hash-map :drug) drug)))

;; Then define reducing function appropriate for this single sequence:

(defn t-test-reducing-function
  [label-a label-b]
  (-> summary-stats
      (update :n (remove nil?)) ;; Only count maps containing the label
      (redux.core/fuse)
      (redux.core/facet [label-a label-b])
      (redux.core/post-complete
       (comp ktest/p-value (partial apply ktest/t-test)))))

(transduce identity (t-test-reducing-function :placebo :drug) data)
;; 0.0018017423704935023

;;Outliers
(def salaries [200 69 141 45 154 169 142 198 178 197 1000000 166 188 178 129 87 151 101 187 154])
(defn confidence-intervals
  "Pick a random sample with replacement calculate the statistic (difference in avgs)"
  [percentile num-bootstraps salaries tails]
  (let [edge (/ (- 1 percentile) tails)
        lower-index (Math/ceil (* edge num-bootstraps))
        upper-index (Math/floor (* (- 1 edge) num-bootstraps))
        diffs (-> (repeatedly num-bootstraps #(bootstrap-avg salaries))
                  sort
                  vec)]
    {:lower (float (nth diffs lower-index))
     :upper (float (nth diffs upper-index))
     :min (float (first diffs))
     :max (float (last diffs))
     :avg (float (avg diffs))
     #_:raw-data #_diffs}))
(confidence-intervals 0.90 10000 salaries 2)

;;(fstat/ttest-two-samples drug placebo)

;;### Terminology used in the namespace
;; - Binomial distribution: the binomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments
;; Null hypothesis - The normal boring expectation of an observation.
;; - Significance: (p-value) The ability to say wether an observation has not happened due to random chance. A very small p-value(probability) indicates that the observation observed is not probable so the null hypothesis can be rejected. 
;; - Bootstrapping: Drawing from a sample at random with replacement
;; - Confidence interval: The range of values of the measure we expect our test statistic is likely to take
;; - Z-Test - The test used to compare two means used as a measure to create the p-value to accept or reject the null hypothesis when the standard deviation is known
;; - Students T - Test - Similar to the z-test  Used to compare the means for a smaller number of samples with the assumption that the distribution is normal and the standard deviation is unknown

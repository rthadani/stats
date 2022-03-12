;# Introduction to the measures of statistical inference
;Useful inferential statistics does not have to be just the domain of data 
;scientists. This workshop follows examples in the book *Statistics is Easy* by 
;*Dennis Shasha* and *Manda Wilson*, along with other real 
;world examples to demonstrate concepts of fairness, p-value, confidence
;intervals, power. All concepts will be explained purely using functions from the 
;core clojure library.

;Few people remember statistics and often have to refer to books to apply formulae,
;hoping that the underlying distribution and its assumptions satisfy the needs of
;the formula, however the magic and the assumptions can be avoided through 
;randomization techniques known as resampling

;We try to answer the questions for the results of an experiment, how accurate is
;the measurement likely to be (confidence interval) and could it have happened by
;mistake

;## Setup
;- [hanami](https://github.com/jsa-aerial/hanami) wrapper for vega lite
;- [clerk](https://github.com/nextjournal/clerk) which displays this namespace as a notebook 
;- [kixi-stats](https://github.com/MastodonC/kixi.stats) to verify stats using classical statistics  
(ns statistics-is-easy.workshop-1-1
  "First of the two workshops on statistical inference"
  (:require [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [kixi.stats.core :as kcore]
            [kixi.stats.distribution :as kstatsd]
            [kixi.stats.test :as ktest]
            [nextjournal.clerk :as clerk]))
;## Experiment 1
;Is a coin fair if I told you that I observed 15 heads in 17 coin tosses  
;### Terminology used
;- Null hypothesis - The boring hypothesis or what normally occurs the coin is fair
;- Alternate hypothesis - The coin isnt fair the observation you see cannot happen 
;by mistake
;- Resampling - To take a given sample and create new samples
;- p-value - measure how unlikely it is to observe the alternate hypothesis 
;- Binomial distribution - Type of distribution where a test has two possible outcomes 
;- Z-test - Measure the p-value using a statistical method 

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

(apply-prob 0.5 17)

(repeatedly 10  #(apply-prob 0.5 17) )

;Lets draw out the distribution
(defn render-one
  "Render one experiment where xy is the data x is the domain (input in the coin case the the number of heads in one experiment) 
  and y is constantly 1. The ys are summed up for every x"
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

(->> (repeatedly 10000 #(apply-prob 0.5 17))
     (map (fn [s] {:x s :y 1}))
     (render-one "blue" "number of samples" "number of heads out of 17 tossses")
     clerk/vl)

;Now using a binomial distribution
(->> (kstatsd/sample 10000 (kstatsd/binomial {:n 17 :p 0.5}))
     (map (fn [s] {:x s :y 1}))
     (render-one "blue" "number of heads out of 17 tossses" "number of samples")
     clerk/vl)

;#### Encode the experiment

;Is the coin fair.
;Lets draw both the distributions first
(defn render-ab
  [a-prob b-prob trials y-label]
  (-> (hc/xform ht/layer-chart
                :LAYER [(render-one "blue" "A" y-label 
                                    (map (fn [s] {:x s :y 1}) 
                                         (repeatedly 10000 #(apply-prob a-prob trials)))) 
                        (render-one "red" "B" "" 
                                    (map (fn [s] {:x s :y 1}) 
                                         (repeatedly 10000 #(apply-prob b-prob trials))))])
      clerk/vl))

; (render-ab 0.5 (/ 15 17) 17 "number of samples")

;Encode the experiment - the result is the p-value which says how unlikely the alternate hypothesis is
(defn experiment
  [observed trials success-probability num-bootstraps]
  (let [experimental-successes (repeatedly num-bootstraps 
                                           #(apply-prob success-probability trials))
        count-good (count (filter #(>= % observed) experimental-successes))]
    {:count-good count-good
     :num-bootstraps num-bootstraps
     :observed-probability (float (/ count-good num-bootstraps))}))

(experiment 15 17 0.5 10000)

;Lets imagine doing this with the option where lets say 13 out of 17 flips is the normal
(experiment 15 17 (/ 13 17) 10000)

;#### Do the same with a z-test

; the mean 

; $$ 0.5 * population $$

; the standard deviation where p is the probability of success

; $$ \sqrt{p * (1 - p)} $$  

  (def population 17)
  ;(def initial-probability (float (/ 13 17)))
  (def initial-probability 0.5)
  (ktest/p-value (ktest/simple-z-test {:mu (* population initial-probability) 
                                       :sd (Math/sqrt (* population initial-probability (- 1 initial-probability)))}
                                    {:mean 15 :n population}))


;#### Other examples

;Take the instance of an online game company that introduces a new feature 
;where the normal retention rate is 70% after introducing the feature we observe
;that 38 of 97 people revisit the site. Is there a real problem with this feature?
(render-ab 0.7 (float (/ 38 97)) 97 "number of samples")
(experiment 70 100 (float (/ 38 97)) 10000)
;What are assumptions we made here

;
;When doing an A/B experiment the normal click through rate(Number of users clicking impressions)  is 2%. 
; The adtech company has changed the way ads are being selected for display and observes that of the 900 impressions presented 30 users clicked into the ads. 
; Is the new selection system better
(render-ab 0.02 (float (/ 30 900)) 900 "number of samples")
(experiment 30 900 0.02 10000)


;Let us assume you have 1000 users, 550 were directed to site A, 450 to site B. 
;In site A, 48 users converted. In site B, 56 users converted. Is this a statistically significant result? 
(render-ab (float (/ 48 550)) (float (/ 56 450)) 450 "number of samples")

;## Experiment 2 
;Test the effectiveness of a drug
;
;So far we have been  doing true false test where we did not have continuous values.
;Lets look at a case where we can take a range of values and test is there a difference
;between the two groups of values

;### Terminology 
;- Labelled data - The known value of the result for each test
;- Resampling - Randomly shuffling labels on the data.
;- Placebo - A drug that has no real effect but this is unknown to the person taking the drug
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

;Lets look at the average difference between the drug and the placebo 

(avg-diff drug placebo)

;Was this effect seen by pure chance.
;
;Lets run an experiment where many many times we shuffle the labels and check the difference in the average for each shuffled drug. 
;Each time we see a value greater than the observed we increment a counter. 
(defn shuffled-avg-diff
  [treatment control]
  (let [all-together (shuffle (concat treatment control))
        new-treatment (take (count treatment) all-together)
        new-control (drop (count treatment) all-together)]
    (avg-diff new-treatment new-control)))

(shuffled-avg-diff drug placebo)

(->> (repeatedly 10000 #(shuffled-avg-diff drug placebo))
              (map (fn [s] {:x s :y 1}))
              (render-one "blue" "difference between means" "bootstrap samples")
              clerk/vl)

;### Why we shuffle
;
;The idea here is if the drug had no real effect then the placebo would often give more improvement than the drug.
;
;By shuffling we are simulating a situation in which some placebo measurements replace the drug measurements. 
;
;If the observed average difference is matched or exceeded then the drug may have no visible effect beyond the placebo
;
;We will look at this by looking at the more _degenerate_ case where we artificially create the difference from a population 
;We take  random widely varying values for a population and simply add the observed difference to each value for the drug.
(defn drug-experiment
  [num-bootstraps drug placebo]
  (let [observed-diff (avg-diff drug placebo)
        experiment-diff (repeatedly num-bootstraps #(shuffled-avg-diff drug placebo))
        count-good (->> experiment-diff
                        (filter #(>= % observed-diff))
                        count)
        observed-probability (float (/ count-good num-bootstraps))]
    {:num-bootstraps num-bootstraps
     :count-good count-good
     :observed-probability observed-probability}))

(drug-experiment 10000 drug placebo)

(def placebo-measure-2 [56 348 162 420 440 250 389 476 288 456])
(def drug-measure-3 (mapv #(+ % (rand-int 50)) placebo-measure-2))

(avg-diff drug-measure-3 placebo-measure-2)
(drug-experiment 10000 drug-measure-3 placebo-measure-2)



(->> (repeatedly 10000 #(shuffled-avg-diff drug-measure-3 placebo-measure-2))
              (map (fn [s] {:x s :y 1}))
              (render-one "blue" "difference between means" "bootstrap samples")
              clerk/vl)

;Lets look at the results by using a T-test with the kixi stats library.
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

;## Confidence intervals
;Is the drug effective (How large is the effect). Lets say the average survival on the placebo is 5 years, and the 
;drug increases the survival on an average by 3 days. The difference between 5 years and 3 days may be significant
;but it is not a large effect
;
;To measure the confidence interval we 
;1. bootstrap - choose uniformly at random with replacement for the drug and the placebo
;2. Add the observed difference for this instance of the values to the result
;3. Repeat 1 and 2 many many times
;4. Sort the results and pick the ends based on the desired confidence interval
(defn bootstrap-avg
  [sample]
  (-> (repeatedly (count sample) #(rand-nth sample))
      avg))

(defn confidence-intervals
  [percentile num-bootstraps treatment control tails]
  (let [edge (/ (- 1 percentile) tails)
        lower-index (Math/ceil (* edge num-bootstraps))
        upper-index (Math/floor (* (- 1 edge) num-bootstraps))
        diffs (-> (repeatedly num-bootstraps #(- (bootstrap-avg treatment) (bootstrap-avg control)))
                  sort)]
    {:lower (float (nth diffs lower-index))
     :upper (float (nth diffs upper-index))
     :min (first diffs)
     :max (last diffs)
     :avg (avg diffs)}))

;(confidence-intervals 0.9 10000 drug placebo 2)

(def xrule-chart
  (-> (assoc-in ht/xrule-layer [:encoding :x2] {:field :X2})
      (assoc :data ht/data-options)))
(defn render-ci
  [ci]
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
                        #_ (-> (hc/xform ht/point-chart
                                      :X "avg"
                                      :YBIN hc/RMV
                                      :VALDATA [ci]
                                      :SIZE 14
                                      :MCOLOR "red"
                                      :TOOLTIP hc/RMV)
                            (update :encoding dissoc :y)
                            (assoc :tooltip [{:field "avg"}]))]
                :HEIGHT 40)))

(->> (confidence-intervals 0.90 10000 drug placebo 2) 
     render-ci
     clerk/vl)

;## Power
;The power of a test is the probability of rejecting the null hypothesis when it is false 
(defn is-significant?
  [{:keys [observed-probability]}]
  (<= observed-probability 0.05))

(defn power
  [experiment & [p]]
  (->> (range 1000) ;;do the following 1000 times
       (pmap #(experiment %)) ;;run the experiment
       (filter is-significant?) ;;filter out successes - experiments where the null hypothesis is false
       count ;;count them
       ((fn [c] (if p (< (* p 1000) c) (/ c 1000)))))) ;;See if it rejects the null hypothesis atleast power times number of tries

;(power (fn [exp-num] (drug-experiment 10000 drug placebo)))

;Can i reject the null hypothesis with 80% probability

;(power (fn [exp-num] (drug-experiment 10000 drug placebo)) 0.8)

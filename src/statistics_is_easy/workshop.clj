(ns statistics-is-easy.workshop
  (:require [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [kixi.stats.distribution :as kstatsd]
            [kixi.stats.test :as ktest]
            [fastmath.stats :as fstat]
            [kixi.stats.core :as kcore]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind] ; a collection of known kinds of notes
            [scicloj.kindly.api :as kindly]
            [scicloj.viz.api :as viz]))

(comment
  (notespace/restart! {})
  (notespace/restart-events!)
  (notespace/stop!))

;;## Is a coin fair if I told you that I observed 15 heads in 17 coin tosses  

; Create a large number of buckets to draw from  and split them up based on the p parameter.
; Any random drawing that draws from the left side is considered a success

;render the successes

;do the same by using a binomial distribution from the kixi stats library

;is the coin fair

; ## Test to check the effectiveness of a drug
; To check the effectiveness of a drug we test it against a placebo, the numbers indicate the measured improvement
(def placebo [54 51 58 44 55 52 42 47 58 46])
(def drug [54 73 53 70 73 68 52 65 65])

;what is the observed difference in mean

;Is the drug effective
;Lets shuffle the labels, and check the average differences. 

;Why we shuffle 

;Can the drug be put into the market

;### Confidence intervals

;### De-generate case

;## Power

;## Terminology used in the namespace
; - Binomial distribution: the binomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments
; Null hypothesis - The normal boring expectation of an observation.
; - Significance: (p-value) The ability to say wether an observation has not happened due to random chance. A very small p-value(probability) indicates that the observation observed is not probable so the null hypothesis can be rejected. Done by shuffling 
; - Bootstrapping: Drawing from a sample at random with replacement
; - Confidence interval: The range of values of the measure we expect our test statistic is likely to take
; - Z-Test - The test used to compare two means used as a measure to create the p-value to accept or reject the null hypothesis when the standard deviation is known
; - Students T - Test - Similar to the z-test  Used to compare the means for a smaller number of samples with the assumption that the distribution is normal and the standard deviation is unknown

;Practical considerations
;- Bootstrapping - When sample sizes are small (under 100) we may under estimate the size of the confidence interval and a significance test may work better
;- Significance/shuffle/permutations can be used for as few as 3 points
;- Strategies for evaluating treatments - CI, significance test, shuffle like significance, resample
;- Bootstrapping should be used with caution when there are outliers, maybe use rank transformations
;- Neither bootstrapping nor sampling should be used if the sample is not representative
;- Resampling should be used with care when data exhibits serial dependence

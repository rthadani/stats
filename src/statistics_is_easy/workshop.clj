(ns statistics-is-easy.workshop
  (:require [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [kixi.stats.distribution :as kstatsd]
            [kixi.stats.test :as ktest]
            [fastmath.stats :as fstat]
            [kixi.stats.core :as kcore]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind]
            [scicloj.kindly.api :as kindly]
            [scicloj.viz.api :as viz]))

^kind/hidden
(comment

  (notespace/restart! {:notes?     true
                       :header?    false
                       :last-eval? false})
  (notespace/restart-events!)
  (notespace/stop!))

;# An introduction to statistical inference

;## Goals
;- Derive conclusions from first principles 
;- Use randomization techniques known collectively as resampling to derive information from the experiment being conducted
;- To answer questions on how accurate a measurement is likely to be and could it have happened by mistake
;- Some real world applications
;- Practical considerations

;
;;## Is a coin fair if I told you that I observed 15 heads in 17 coin tosses  
;
;Create a large number of buckets to draw from  and split them up based on the p parameter. Any random drawing that draws from the left side is
; considered a success
;
;Flip the coin n(17) times (one experiment) Keep track of heads. Repeat this experiment 10000 times
;
;Do the same by using a binomial distribution from the kixi stats library
;
;A **binomial distribution** with parameters n and p is the discrete probability distribution of the number of p successes in a sequence of n
; independent experiments
;
;Render both distributions, one where the mean is 8 and other where the mean is the observed value
;
;Is the coin fair. Encode the experiment

;### Other examples
;Take the instance of an online game company that introduces a new feature where the normal retention rate is 70% after introducing the feature 
;we observe that 38 of 97 people revisit the site. Is there a real problem with this feature?
;
;When doing an A/B experiment the normal click through rate(Number of users clicking impressions)  is 2%. 
; The adtech company has changed the way ads are being selected for display and observes that of the 900 impressions presented 30 users clicked into the ads. 
; Is the new selection system better

; ### Test to check the effectiveness of a drug
; To check the effectiveness of a drug we test it against a placebo, the numbers indicate the measured improvement
(def placebo [54 51 58 44 55 52 42 47 58 46])
(def drug [54 73 53 70 73 68 52 65 65])

;Measure the observed difference in mean between the drug and the placebo. 
;
;Was this effect seen by pure chance.
;
;Lets run an experiment where many many times we shuffle the labels and check the difference in the average for each shuffled drug. 
;Each time we see a value greater than the observed we increment a counter. 
;
;Lets look at the results by using a T-test with the kixi stats library.
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

;Rerun the experiment with the new values and check the significance

;## Confidence intervals
;Is the drug effective (How large is the effect). Lets say the average survival on the placebo is 5 years, and the 
;drug increases the survival on an average by 3 days. The difference between 5 years and 3 days may be significant
;but it is not a large effect
;
;To measure the confidence interval we 
;1. bootstrap - choose uniformly at random with replacement for the drug and the placebo,
;2. Add the observed difference for this instance of the values to the result
;3. Repeat 1 and 2 many many times
;4. Sort the results and pick the ends based on the desired confidence interval

;## Power
;The power of a test is the probability of rejecting the null hypothseis when it is false 

;##  Outliers 
;; Suppose we were measuring the confidence interval of average salaries as a precursor to the evaluation of does college 
;; education affect salary. But we have Bill Gates' salary in the mix
(def salaries [200 69 141 45 154 169 142 198 178 197 1000000 166 188 178 129 87 151 101 187 154])

;## Other evauation methods
;-  Chi-Squared - Measure the deviation of observed data for multiple categories from expectation or test the independence of two variables
;- Fischers Exact test - like chi squared for four categories and expected counts are below 10
; - ANOVA - How different groups are when  independent variable/s is/are changed(One way , multi way)
; - LInear regression
; - Linear corelation - How well a variable  can predict another if a linear relationship exists
; - Multiple Testing -

;## Practical considerations
;- Bootstrapping - When sample sizes are small (under 100) we may under estimate the size of the confidence interval and a significance test may work better
;- Significance/shuffle/permutations can be used for as few as 3 points
;- Strategies for evaluating treatments - CI, significance test, shuffle like significance, resample
;- Bootstrapping should be used with caution when there are outliers
;- Neither bootstrapping nor sampling should be used if the sample is not representative
;- Resampling should be used with care when data exhibits serial dependence.

;## References
;- Statistics is easy - Dennis Shasha, Manda Wilson
;- Bootstrap methods and their application - D.V. Hinkley, A.C. Davidson

;## Terminology used in the namespace
; - Binomial distribution: the binomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments
; - Null hypothesis - The normal boring expectation of an observation.
; - Significance: (p-value) The ability to say wether an observation has not happened due to random chance. A very small p-value(probability) indicates that the observation observed is not probable so the null hypothesis can be rejected. Done by shuffling 
; - Bootstrapping: Drawing from a sample at random with replacement
; - Confidence interval: The range of values of the measure we expect our test statistic is likely to take
; - Z-Test - The test used to compare two means used as a measure to create the p-value to accept or reject the null hypothesis when the standard deviation is known
; - Students T - Test - Similar to the z-test  Used to compare the means for a smaller number of samples with the assumption that the distribution is normal and the standard deviation is unknown

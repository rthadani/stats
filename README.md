# stats

# Statistics is easy
Examples from the book by Denis Shasha, Manda Wilson and Steven G. Krantz done in clojure

This book mainly explains the different statistical measures, the different scenarios under which they are applicable and presents implementing them using re-sampling. Does not assume the underlying distribution for deriving statistical significance  

## Chapter 1 - The basic idea 
- Test of fairness based on a binomial distribution
- Effectiveness of drug done using a t-test. 
- To get a notion of importance of a significant result draw up a confidence interval using bootstrapping

### Chapter 2 - Considerations for resampling
- Bootstrapping - When sample sizes are small (under 100) we may under estimate the size of the confidence interval and a significance test may work better
- Significance/shuffle/permutations can be used for as few as 3 points
- Strategies for evaluating treatments - CI, significance test, shuffle like significance, resample
- Bootstrapping should be used with caution when there are outliers, maybe use rank transformations 
- Neither bootstrapping nor sampling should be used if the sample is not representative
- Resampling should be used with care when data exhibits serial dependence
- Power of a test - used for sample size
- Multi factor designs and blocking
-
### Chapter 3 - Terms


Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

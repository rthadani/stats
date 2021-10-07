(ns statistics-is-easy.chapter-1
  (:require
   [tablecloth.api :as table]
   [tech.v3.dataset :as dataset]
   [scicloj.viz.api :as viz]
   [aerial.hanami.common :as hc]
   [aerial.hanami.templates :as ht]
   [tech.viz.vega]
   [scicloj.notespace.v4.api :as notespace.v4]
   [scicloj.notespace.v4.render :as v4.render]
   [tech.v3.dataset.math :as ds-math]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [clojure.string :as str]
   [fastmath.stats :as stats]
   [fastmath.random :as rnd]
   [fitdistr.core :as fit]
   [kixi.stats.core :as kstats]
   [kixi.stats.distribution :as kstatsd]))

(comment
  (notespace.v4/start {})
  (notespace.v4/stop {}))

(defn apply-prob
  [p n]
  (let [draw-space (-> (/ 1 p) (* 1000) (+ 0.05))]
    (reduce (fn [success draw]
              (if (>= (* p draw-space)  draw)
                (inc success)
                success))
            0
            (repeatedly n #(rand draw-space)))))

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

;;Do an experiment where we see 15 out of 17 heads in a fair coin
(let [{:keys [count-good num-bootstraps observed-probability]} (experiment 15 17 0.5 10000)]
  (print (format "%d out of %d times we got atleast the %d number of heads in %d tosses\nProbability that chance alone gave us atleast %d heads in %d tosses is %f" count-good num-bootstraps 15 17 15 17 observed-probability)))

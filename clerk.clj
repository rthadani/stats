(ns clerk-starter
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:browse? true
               :watch-paths ["src"]})

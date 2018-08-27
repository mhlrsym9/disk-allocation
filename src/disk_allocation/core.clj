(ns disk-allocation.core
  (:require [disk-allocation.first-attempt :as first-attempt])
  (:require [disk-allocation.second-attempt :as second-attempt])
  (:gen-class))

(defn -main []
  (first-attempt/find-cheapest-system))



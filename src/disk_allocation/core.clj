(ns disk-allocation.core
  (:require [disk-allocation.first-attempt :as first-attempt])
  (:gen-class))

(defn -main []
  (first-attempt/find-cheapest-system))



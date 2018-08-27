(ns disk-allocation.third-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo])
  (:import (disk_allocation.data Machine)))

(def max-drives 16)

(defn- valid-drive-arrays [number-drives]
  (filter (fn [da]
            (= 0 (mod number-drives
                      (:number-drives (first (first da))))))
          all-drive-arrays))

(defn- create-all-drive-array-configurations [number-drives valid-drive-arrays]
  (mapcat (fn [da] (combo/selections da
                                     (/ number-drives
                                        (:number-drives (first (first da))))))
          valid-drive-arrays))

(defn- create-sorted-drive-array-configurations [max-number-drives]
  (mapcat (fn [number-drives] (create-all-drive-array-configurations number-drives
                                                                     (valid-drive-arrays number-drives)))
          (range 2 (inc max-number-drives))))

(def sizes (list lan-client-target-size
                 lan-server-target-size
                 lan-combined-target-size
                 dmz-client-target-size
                 dmz-server-target-size
                 dmz-combined-target-size))

(defn- calculate-dac-size [dac]
  (reduce (fn [r da] (+ r (:tib-50-percent da))) 0.00M dac))

(defn- dac-right-size? [size dac]
  (let [dac-size (calculate-dac-size dac)]
    (cond
      (= size lan-client-target-size) (and (<= lan-client-target-size dac-size)
                                           (<= dac-size (* 1.2M lan-client-target-size)))
      (= size lan-server-target-size) (and (<= lan-server-target-size dac-size)
                                           (<= dac-size (* 1.2M lan-server-target-size)))
      (= size lan-combined-target-size) (and (<= lan-combined-target-size dac-size)
                                             (<= dac-size (* 1.1M lan-combined-target-size)))
      (= size dmz-client-target-size) (and (<= dmz-client-target-size dac-size)
                                           (<= dac-size (* 6.0M dmz-client-target-size)))
      (= size dmz-server-target-size) (and (<= dmz-server-target-size dac-size)
                                           (<= dac-size (* 2.0M dmz-server-target-size)))
      (= size dmz-combined-target-size) (and (<= dmz-combined-target-size dac-size)
                                             (<= dac-size (* 2.0M dmz-combined-target-size)))
      :else false)))

(defn all-drive-array-configurations-validated-by-size [max-number-drives storage-size]
  (filter #(dac-right-size? storage-size %) (create-sorted-drive-array-configurations max-number-drives)))

(defn- dac-cost [dac]
  (reduce (fn [r {:keys [number-drives], :keys [drive-cost] :drive}]
            (+ r (* number-drives drive-cost)))
          0.00M
          dac))

(defn- number-drives-in-dac [{:keys [drive-size]} dac]
  (reduce (fn [r da] (+ r (if (= drive-size (:drive-size (:drive da)))
                            (:number-drives da)
                            0))) dac))

(defn- number-drives-in-sc [drive sc]
  (reduce (fn [r dac] (+ r (number-drives-in-dac drive dac))) sc))

(defn- calculate-system-configuration-cost [sc]
  (- (reduce (fn [r dac] (+ r (dac-cost dac))) sc)
     (* (:drive-cost four-tb-drive) (min (number-drives-in-sc four-tb-drive sc) 9))
     (* (:drive-cost one-tb-drive) (min (number-drives-in-sc one-tb-drive sc) 4))))

(defn- find-cheapest-storage-configuration [mcs]
  (reduce (fn [[cheapest-cost-sc :as cheapest] sc] (let [cost-sc (calculate-system-configuration-cost sc)]
                                                                 (if (< cost-sc cheapest-cost-sc)
                                                                   (list cost-sc sc)
                                                                   cheapest)))
          (combo/cartesian-product (for [[number-drives storage-size] mcs]
                                     (all-drive-array-configurations-validated-by-size number-drives storage-size)))))

(def find-cheapest-storage-configuration-memo (memoize find-cheapest-storage-configuration))
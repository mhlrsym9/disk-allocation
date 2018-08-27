(ns disk-allocation.second-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo])
  (:import (disk_allocation.data Machine)))

(comment (defn- maximum-target-size [the-target-size]
           (cond
             (> 1.0M the-target-size) 3.0M
             (> 2.0M the-target-size) 4.0M
             (> 4.0M the-target-size) 6.0M
             (> 8.0M the-target-size) 10.0M
             :else (* 1.2M the-target-size))))

(defn- maximum-target-size [the-target-size]
  (* 1.2M the-target-size))

(defn- generate-all-valid-machines [{:keys [case mb hba], [the-target-size] :target-size} percent-key drive-arrays]
  (let [maximum-number-of-drives-in-case (+ (:three-point-five-drives case)
                                            (:two-point-five-drives case))
        maximum-number-of-drives-on-mb (+ (:number-sata-connections mb)
                                          (:additional-sata-connectors hba))
        block-range (inc (int (Math/floor (/ (min maximum-number-of-drives-in-case
                                                  maximum-number-of-drives-on-mb)
                                             (:number-drives (first drive-arrays))))))
        all-dacs (:all-drive-arrays-configurations (first (filter (fn [{:keys [br]}] (= block-range br))
                                                                  (filter (fn [{:keys [das]}] (= drive-arrays das))
                                                                          all-drive-array-configurations))))
        configurations-with-right-physical-drive-sizes (filter (partial utils/is-right-physical-drive-size case)
                                                               all-dacs)
        all-valid-machines (filter #(let [drive-array-configuration-size (reduce (fn [r v] (+ r (percent-key v))) 0 %)]
                                      (and (<= the-target-size drive-array-configuration-size)
                                           (>= (maximum-target-size the-target-size) drive-array-configuration-size)))
                                   configurations-with-right-physical-drive-sizes)]
    all-valid-machines))

(defn- generate-all-valid-storage-machines [^Machine sm]
  (map (fn [dac] {:drive-array-configuration dac :storage-machine sm})
       (mapcat (fn [das] (generate-all-valid-machines sm :tib-50-percent das)) all-drive-arrays)))

(defn- drive-array-total-cost [{:keys [number-drives], {:keys [drive-cost]} :drive}]
  (* number-drives drive-cost))

(defn- machine-total-cost [^Machine machine]
  (+ (:cost (:case machine))
     (:cost (:mb machine))
     (:cost (:cpu machine))
     (:cost (:hba machine))))

(def drive-adjustments (list (list one-tb-drive 4)
                             (list four-tb-drive 9)))

(defn- count-matching-drives-in-system-configuration [vsmc {:keys [drive-size]}]
  (count (filter (fn [ds] (= drive-size ds))
                 (mapcat (fn [{:keys [number-drives], {:keys [drive-size]} :drive}]
                           (repeat number-drives drive-size))
                         (mapcat :drive-array-configuration vsmc)))))

(defn- adjust-cost-for-matching-drives-in-system-configuration [vsmc {:keys [drive-cost] :as drive} held]
  (* drive-cost
     (min held (count-matching-drives-in-system-configuration vsmc drive))))

(defn- storage-machine-total-cost [{:keys [drive-array-configuration storage-machine]}]
  {:drive-cost (reduce (fn [r v] (+ r (drive-array-total-cost v))) 0.00M drive-array-configuration)
   :machine-cost (machine-total-cost storage-machine)})

(defn- generate-all-valid-storage-machine-configurations [smc]
  (let [drive-heuristic (- 3000.00M (* 300M (- (count smc) 2)))]
    (filter (fn [{:keys [drive-cost]}] (< drive-cost drive-heuristic))
            (map (fn [vsmc] (let [{:keys [drive-cost machine-cost]} (reduce (fn [r v] (merge-with + r (storage-machine-total-cost v)))
                                                                            {:drive-cost 0.00M :machine-cost 0.00M}
                                                                            vsmc)
                                  drive-adjustment (apply + (map (fn [[drive held]]
                                                                   (adjust-cost-for-matching-drives-in-system-configuration vsmc drive held))
                                                                 drive-adjustments))]
                              {:vsmc       vsmc
                               :cost       (- (+ drive-cost machine-cost) drive-adjustment)
                               :drive-cost (- drive-cost drive-adjustment)}))
                 (apply combo/cartesian-product (map #(generate-all-valid-storage-machines %) smc))))))

(defn- does-cpu-match? [^Machine machine cpu]
  (= (:name (:cpu machine)) (:name cpu)))

(defn- does-hba-match? [^Machine machine hba]
  (= (:name (:hba machine)) (:name hba)))

(defn- does-mb-match? [^Machine machine mb]
  (= (:name (:mb machine)) (:name mb)))

(defn- does-case-match? [^Machine machine case]
  (= (:name (:case machine)) (:name case)))

(defn- count-matching-items-in-system-configuration [[{:keys [vsmc]} lan dmz] item does-item-match-fnc?]
  (count (filter (fn [^Machine machine] (does-item-match-fnc? machine item))
                 (concat (list lan dmz) (map #(:storage-machine %) vsmc)))))

(defn- adjust-cost-for-matching-items-in-system-configuration [sc item does-item-match-fnc? held]
  (* (:cost item)
     (min held (count-matching-items-in-system-configuration sc item does-item-match-fnc?))))

(def adjustments (list (list e5-2603-v3 does-cpu-match? 1)
                       (list e5-2603-v4 does-cpu-match? 1)
                       (list g3900 does-cpu-match? 1)
                       (list atom-c2750 does-cpu-match? 1)
                       (list hba-9211-8i does-hba-match? 1)
                       (list asrock-x99m does-mb-match? 1)
                       (list ga-9sisl does-mb-match? 1)
                       (list one-r5 does-case-match? 2)
                       (list one-phanteks-itx does-case-match? 1)
                       (list one-silencio does-case-match? 1)))

(defn- storage-configuration-total-cost [[{:keys [cost]} lan dmz :as sc]]
  (- (+ cost
        (machine-total-cost lan)
        (machine-total-cost dmz))
     (apply + (map (fn [[item match-fnc held]]
                     (adjust-cost-for-matching-items-in-system-configuration sc item match-fnc held))
                   adjustments))
     ; Adjust for extra memory in augmented machine.
     (let [number-msi (count-matching-items-in-system-configuration sc msi-x99a-tomahawk does-mb-match?)
           number-augmented-msi (count-matching-items-in-system-configuration sc augmented-msi-x99a-tomahawk does-mb-match?)]
       (cond (< 0 number-msi)
             (:cost msi-x99a-tomahawk)
             (< 0 number-augmented-msi)
             (- (:cost augmented-msi-x99a-tomahawk) (:cost msi-x99a-tomahawk))
             :else 0.00M))))

(defn- storage-configuration-machine-count [sc]
  2)

(defn- extract-machines-of-type [type machines]
  (filter #(= type (:type %)) machines))

(defn- extract-sorted-machine-names [machines]
  (sort (map #(get-in % [:case :name]) machines)))

(defn- good-case-configuration? [machine-names]
  (cond
    (= 1 (count machine-names)) true
    (= 2 (count machine-names)) (= (first machine-names) (second machine-names))
    (= 3 (count machine-names)) (let [the-partitions (partition-by identity machine-names)]
                                  (cond
                                    (= 1 (count the-partitions)) true
                                    (= 2 (count the-partitions)) true
                                    (= 3 (count the-partitions)) false
                                    :else false))
    :else false))

(defn- keep-good-case-configurations [[{:keys [vsmc]} lan dmz]]
  (let [all-machines (concat (map :storage-machine vsmc) (list lan dmz))
        lan-machine-names (extract-sorted-machine-names (extract-machines-of-type :lan all-machines))
        dmz-machine-names (extract-sorted-machine-names (extract-machines-of-type :dmz all-machines))]
    (and (good-case-configuration? lan-machine-names) (good-case-configuration? dmz-machine-names))))

(defn- find-cheapest-system [level apvsc]
  (let [cheapest-system (if (seq apvsc)
                          (reduce (fn [{r-cost :cost, rsc :system-configuration, :as r}
                                       {v-cost :cost, vsc :system-configuration, :as v}]
                                    (cond
                                      (< v-cost r-cost) v
                                      (= v-cost r-cost) (let [r-count (storage-configuration-machine-count rsc)
                                                              v-count (storage-configuration-machine-count vsc)]
                                                          (if (< v-count r-count) v r))
                                      :else r))
                                  apvsc)
                          nil)]
    (if cheapest-system
      (println (str "Cheapest system at " level " level costs " (:cost cheapest-system)))
      (println (str "No systems at all at " level " level!")))
    cheapest-system))

(defn- generate-all-priced-valid-storage-configurations [idx smc]
  (let [all-valid-storage-machine-configurations (generate-all-valid-storage-machine-configurations smc)
        all-system-configurations  (combo/cartesian-product all-valid-storage-machine-configurations
                                                           all-lan-servers
                                                           all-dmz-servers)]
    (println (str "Count of " idx " is " (count all-valid-storage-machine-configurations)))
    (map (fn [[{:keys [vsmc drive-cost]} lan dmz :as sc]]
           {:cost                 (storage-configuration-total-cost sc)
            :drive-cost           drive-cost
            :system-configuration (list vsmc lan dmz)})
         (filter keep-good-case-configurations all-system-configurations))))

(comment (defn- find-the-cheapest-system-for-this-storage-machine-configuration-list [idx smcl]
           (let [futures-list (doall (map-indexed #(future ((comp (partial find-cheapest-system (str "apvsc " %1))
                                                                  generate-all-priced-valid-storage-configurations) %1 %2))
                                                  smcl))
                 cheapest-systems (filter identity (map deref futures-list))]
             (find-cheapest-system (str "smcl " idx) cheapest-systems))))

(defn- find-the-cheapest-system-for-this-storage-machine-configuration-list [idx smcl]
  (let [cheapest-systems (filter identity (map-indexed #((comp (partial find-cheapest-system (str "apvsc " %1))
                                                               generate-all-priced-valid-storage-configurations) %1 %2)
                                                       smcl))
        cheapest-system (find-cheapest-system (str "smcl " idx) cheapest-systems)]
    (println (str "Drive cost is " (:drive-cost cheapest-system)))
    cheapest-system))

(defn- find-the-cheapest-system []
  (find-cheapest-system "lasmcl" (filter identity (map-indexed #((comp (partial find-cheapest-system (str "tlsmcl " %1))
                                                               find-the-cheapest-system-for-this-storage-machine-configuration-list) %1 %2)
                                                       list-of-all-storage-machine-configuration-lists))))

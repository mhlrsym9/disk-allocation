(ns disk-allocation.third-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo])
  (:import (disk_allocation.data Machine)))

(defn- extract-valid-drive-arrays [number-drives-needed]
  (filter (fn [[{:keys [number-drives]}]]
            (= 0 (mod number-drives-needed number-drives)))
          all-drive-arrays))

(defn- create-all-drive-array-configurations [number-drives-needed valid-drive-arrays]
  (mapcat (fn [[{:keys [number-drives]} :as das]]
            (distinct (map (fn [dac] (sort-by :tib-50-percent dac))
                           (combo/selections das (/ number-drives-needed number-drives)))))
          valid-drive-arrays))

(defn- create-sorted-drive-array-configurations [{:keys [max-number-drives]}]
  (mapcat (fn [number-drives] (create-all-drive-array-configurations number-drives
                                                                     (extract-valid-drive-arrays number-drives)))
          (range 2 (inc max-number-drives))))

(defn- is-dac-right? [scp-for-one-component dac percent-key]
  (and (utils/is-dac-right-size? scp-for-one-component dac percent-key)
       (utils/does-dac-have-right-physical-drive-size-configuration? scp-for-one-component dac)))

(defn- all-drive-array-configurations-validated-by-size [scp-for-one-component percent-key]
  (filter #(is-dac-right? scp-for-one-component % percent-key)
          (create-sorted-drive-array-configurations scp-for-one-component)))

(defn- dac-cost [dac]
  (reduce (fn [r {:keys [number-drives], {:keys [drive-cost]} :drive}]
            (+ r (* number-drives drive-cost)))
          0.00M
          dac))

(defn- calculate-storage-configuration-cost [sc scp]
  (- (reduce (fn [r dac] (+ r (dac-cost dac))) 0.00M sc)
     (utils/calculate-drive-adjustment sc scp)))

(defn- find-cheapest-storage-configuration [scp]
  (let [all-storage-configurations (apply combo/cartesian-product
                                          (map (fn [scp-for-one-component]
                                                 (all-drive-array-configurations-validated-by-size scp-for-one-component :tib-50-percent)) scp))]
    (when (seq all-storage-configurations)
      (second (reduce (fn [[cheapest-cost-sc :as cheapest] sc] (let [cost-sc (calculate-storage-configuration-cost sc scp)]
                                                                 (if (< cost-sc cheapest-cost-sc)
                                                                   (list cost-sc sc)
                                                                   cheapest)))
                      (list (calculate-storage-configuration-cost (first all-storage-configurations) scp)
                            (first all-storage-configurations))
                      (rest all-storage-configurations))))))

(def find-cheapest-storage-configuration-memo (memoize find-cheapest-storage-configuration))




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

(defn- keep-good-case-configurations [[smc lan dmz]]
  (let [all-machines (concat smc (list lan dmz))
        lan-machine-names (extract-sorted-machine-names (extract-machines-of-type :lan all-machines))
        dmz-machine-names (extract-sorted-machine-names (extract-machines-of-type :dmz all-machines))]
    (and (good-case-configuration? lan-machine-names) (good-case-configuration? dmz-machine-names))))

(defn- calculate-machine-cost [^Machine machine]
  (+ (:cost (:case machine))
     (:cost (:mb machine))
     (:cost (:cpu machine))
     (:cost (:hba machine))))

(defn- does-cpu-match? [^Machine machine cpu]
  (= (:name (:cpu machine)) (:name cpu)))

(defn- does-hba-match? [^Machine machine hba]
  (= (:name (:hba machine)) (:name hba)))

(defn- does-mb-match? [^Machine machine mb]
  (= (:name (:mb machine)) (:name mb)))

(defn- does-case-match? [^Machine machine case]
  (= (:name (:case machine)) (:name case)))

(defn- count-matching-items-in-machine-configuration [[the-storage-machines lan dmz] item does-item-match-fnc?]
  (count (filter (fn [^Machine machine] (does-item-match-fnc? machine item))
                 (concat the-storage-machines (list lan dmz)))))

(defn- adjust-cost-for-matching-items-in-system-configuration [mc item does-item-match-fnc? held]
  (* (:cost item)
     (min held (count-matching-items-in-machine-configuration mc item does-item-match-fnc?))))

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

(defn- calculate-machine-configuration-cost [[the-storage-machines lan dmz :as mc]]
  (- (+ (reduce (fn [cost ^Machine machine] (+ cost (calculate-machine-cost machine))) 0.00M the-storage-machines)
        (calculate-machine-cost lan)
        (calculate-machine-cost dmz))
     (+ (apply + (map (fn [[item match-fnc held]]
                        (adjust-cost-for-matching-items-in-system-configuration mc item match-fnc held))
                      adjustments)))
     (let [number-msi (count-matching-items-in-machine-configuration mc msi-x99a-tomahawk does-mb-match?)
           number-augmented-msi (count-matching-items-in-machine-configuration mc augmented-msi-x99a-tomahawk does-mb-match?)]
       (cond (< 0 number-msi) (:cost msi-x99a-tomahawk)
             (< 0 number-augmented-msi) (- (:cost augmented-msi-x99a-tomahawk) (:cost msi-x99a-tomahawk))
             :else 0.00M))))

(defn- find-cheapest-machine-configuration [smc]
  (let [all-machine-configurations (combo/cartesian-product (list smc) all-lan-servers all-dmz-servers)
        all-valid-machine-configurations (filter keep-good-case-configurations all-machine-configurations)]
    (when (seq all-valid-machine-configurations)
      (second (reduce (fn [[cheapest-cost-mc :as cheapest] mc] (let [cost-mc (calculate-machine-configuration-cost mc)]
                                                                 (if (< cost-mc cheapest-cost-mc)
                                                                   (list cost-mc mc)
                                                                   cheapest)))
                      (list (calculate-machine-configuration-cost (first all-valid-machine-configurations))
                            (first all-valid-machine-configurations))
                      (rest all-valid-machine-configurations))))))

(defn- find-cheapest-storage-system-for-this-storage-machine-configuration [smc]
  (let [storage-configuration-pattern (utils/generate-storage-machine-configuration-pattern smc)
        cheapest-storage-configuration (find-cheapest-storage-configuration-memo storage-configuration-pattern)

        cheapest-machine-configuration (find-cheapest-machine-configuration smc)
        result {:storage-configuration      cheapest-storage-configuration
                :storage-configuration-cost (calculate-storage-configuration-cost cheapest-storage-configuration
                                                                                  storage-configuration-pattern)
                :machine-configuration      cheapest-machine-configuration
                :machine-configuration-cost (calculate-machine-configuration-cost cheapest-machine-configuration)}]
    (println (str "Cheapest cost of this smc is " (+ (:storage-configuration-cost result) (:machine-configuration-cost result))))
    result))

(defn- total-number-drives-in-storage-configuration [sc]
  (reduce (fn [r dac] (+ r (utils/total-number-drives-in-dac dac))) 0 sc))

(defn- find-cheapest-storage-system [level storage-systems]
  (let [cheapest-storage-system (reduce (fn [{r-sc-cost :storage-configuration-cost, r-sc :storage-configuration,
                                              r-mc-cost :machine-configuration-cost, r-mc :machine-configuration, :as r}
                                             {v-sc-cost :storage-configuration-cost, v-sc :storage-configuration,
                                              v-mc-cost :machine-configuration-cost, v-mc :machine-configuration, :as v}]
                                          (let [r-cost (+ r-sc-cost r-mc-cost)
                                                v-cost (+ v-sc-cost v-mc-cost)]
                                            (cond
                                              (< v-cost r-cost) v
                                              (= v-cost r-cost) (let [r-machine-count (count r-mc)
                                                                      v-machine-count (count v-mc)]
                                                                  (cond
                                                                    (< v-machine-count r-machine-count) v
                                                                    (= v-machine-count r-machine-count) (let [r-drive-count (total-number-drives-in-storage-configuration r-sc)
                                                                                                              v-drive-count (total-number-drives-in-storage-configuration v-sc)]
                                                                                                          (if (< v-drive-count r-drive-count)
                                                                                                            v
                                                                                                            r))
                                                                    :else r))
                                              :else r)))
                                        (first storage-systems)
                                        (rest storage-systems))]
    (println (str "Cheapest system at " level " level costs " (+ (:storage-configuration-cost cheapest-storage-system)
                                                                 (:machine-configuration-cost cheapest-storage-system))))
    cheapest-storage-system))

(defn- find-the-cheapest-system-for-this-storage-machine-configuration-list [smcl]
  (let [cheapest-storage-systems (filter identity (map find-cheapest-storage-system-for-this-storage-machine-configuration smcl))]
    (when (seq cheapest-storage-systems)
      (find-cheapest-storage-system "smcl" cheapest-storage-systems))))

(defn- find-the-cheapest-system []
  (let [cheapest-storage-systems (filter identity (map find-the-cheapest-system-for-this-storage-machine-configuration-list
                                                       list-of-all-storage-machine-configuration-lists))]
    (when (seq cheapest-storage-systems)
      (find-cheapest-storage-system "lasmcl" cheapest-storage-systems))))


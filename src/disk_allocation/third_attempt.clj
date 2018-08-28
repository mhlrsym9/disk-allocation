(ns disk-allocation.third-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo])
  (:import (disk_allocation.data Machine)))

(def max-drives 16)

(defn- max-number-drives-for-machine [{{:keys [two-point-five-drives three-point-five-drives]} :case,
                                       {:keys [additional-sata-connectors]}                    :hba,
                                       {:keys [number-sata-connections]}                       :mb}]
  (let [max-number-drives-in-case (+ three-point-five-drives two-point-five-drives)
        max-number-drives-in-mb (+ number-sata-connections
                                   additional-sata-connectors)]
    (if (<= max-number-drives-in-case max-number-drives-in-mb)
      {:max-number-drives            max-number-drives-in-case
       :number-two-point-five-drives two-point-five-drives}
      {:max-number-drives            max-number-drives-in-mb
       :number-two-point-five-drives (max 0 (- number-two-point-five-drives
                                               (- max-number-drives-in-case
                                                  max-number-drives-in-mb)))})))

(defn- target-size-for-machine [{:keys [target-size]}]
  {:target-size (first target-size)})

(defn- generate-storage-configuration-pattern [smc]
  (map (fn [mc] (merge (max-number-drives-for-machine mc) (target-size-for-machine mc))) smc))

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

(defn- create-sorted-drive-array-configurations [{:keys [max-number-drives]}]
  (mapcat (fn [number-drives] (create-all-drive-array-configurations number-drives
                                                                     (valid-drive-arrays number-drives)))
          (range 2 (inc max-number-drives))))

(defn- calculate-dac-size [dac]
  (reduce (fn [r da] (+ r (:tib-50-percent da))) 0.00M dac))

(defn- is-dac-right-size? [{:keys [target-size]} dac]
  (let [dac-size (calculate-dac-size dac)]
    (cond
      (= target-size lan-client-target-size) (and (<= lan-client-target-size dac-size)
                                                  (<= dac-size (* 1.2M lan-client-target-size)))
      (= target-size lan-server-target-size) (and (<= lan-server-target-size dac-size)
                                                  (<= dac-size (* 1.2M lan-server-target-size)))
      (= target-size lan-combined-target-size) (and (<= lan-combined-target-size dac-size)
                                                    (<= dac-size (* 1.1M lan-combined-target-size)))
      (= target-size dmz-client-target-size) (and (<= dmz-client-target-size dac-size)
                                                  (<= dac-size (* 6.0M dmz-client-target-size)))
      (= target-size dmz-server-target-size) (and (<= dmz-server-target-size dac-size)
                                                  (<= dac-size (* 2.0M dmz-server-target-size)))
      (= target-size dmz-combined-target-size) (and (<= dmz-combined-target-size dac-size)
                                                    (<= dac-size (* 2.0M dmz-combined-target-size)))
      :else false)))

(defn- number-of-two-point-five-drives [dac]
  (reduce (fn [r {:keys [number-drives], {:keys [can-be-two-point-five-drive]} :drive}]
            (+ r (if can-be-two-point-five-drive
                   number-drives
                   0))) 0 dac))

(defn- does-dac-have-right-physical-drive-size-configuration? [{:keys [number-two-point-five-drives]} dac]
  (if (< 0 number-two-point-five-drives)
    (<= number-two-point-five-drives (number-of-two-point-five-drives dac))
    true))

(defn- is-dac-right? [scp-for-one-component dac]
  (and (is-dac-right-size? scp-for-one-component dac)
       (does-dac-have-right-physical-drive-size-configuration? scp-for-one-component dac)))

(defn all-drive-array-configurations-validated-by-size [{:keys [target-size] :as scp-for-one-component}]
  (filter #(is-dac-right? target-size %) (create-sorted-drive-array-configurations scp-for-one-component)))

(defn- dac-cost [dac]
  (reduce (fn [r {:keys [number-drives], :keys [drive-cost] :drive}]
            (+ r (* number-drives drive-cost)))
          0.00M
          dac))

(defn- number-drives-in-dac [{:keys [drive-size]} dac]
  (reduce (fn [r da] (+ r (if (= drive-size (:drive-size (:drive da)))
                            (:number-drives da)
                            0))) dac))

(defn- number-drives-in-storage-configuration [drive sc]
  (reduce (fn [r dac] (+ r (number-drives-in-dac drive dac))) sc))

(defn- total-number-drives-in-dac [dac]
  (reduce (fn [r da] (+ r (:number-drives da))) dac))

(defn- calculate-storage-configuration-cost [sc scp]
  (let [number-one-tb-two-point-five-drives-needed (apply + (map (fn [dac {:keys [max-number-drives number-two-point-five-drives]}]
                                                                   (let [number-drives (total-number-drives-in-dac dac)]
                                                                     (max 0 (- number-drives
                                                                               (- max-number-drives
                                                                                  number-two-point-five-drives)))))
                                                                 sc scp))
        number-one-tb-drives-needed (number-drives-in-storage-configuration one-tb-drive sc)
        number-one-tb-three-point-five-drives-needed (- number-one-tb-drives-needed number-one-tb-two-point-five-drives-needed)
        number-one-tb-two-point-five-drives-used-in-dac (min number-one-tb-two-point-five-drives-needed 2)
        number-additional-one-tb-two-point-five-drives (- 2 number-one-tb-two-point-five-drives-used-in-dac)]
    (- (reduce (fn [r dac] (+ r (dac-cost dac))) sc)
       (* (:drive-cost four-tb-drive) (min (number-drives-in-storage-configuration four-tb-drive sc) 9))
       (* (:drive-cost one-tb-drive) (min number-one-tb-three-point-five-drives-needed (+ 2 number-additional-one-tb-two-point-five-drives)))
       (* (:drive-cost one-tb-drive) (min number-one-tb-two-point-five-drives-needed 2)))))

(defn- find-cheapest-storage-configuration [scp]
  (let [all-storage-configurations (apply combo/cartesian-product
                                          (map (fn [scp-for-one-component]
                                                 (all-drive-array-configurations-validated-by-size scp-for-one-component)) scp))]
    (when (seq all-storage-configurations)
      (reduce (fn [[cheapest-cost-sc :as cheapest] sc] (let [cost-sc (calculate-storage-configuration-cost sc scp)]
                                                         (if (< cost-sc cheapest-cost-sc)
                                                           (list cost-sc sc)
                                                           cheapest)))
              all-storage-configurations))))

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

(defn- keep-good-case-configurations [[{:keys [vsmc]} lan dmz]]
  (let [all-machines (concat (map :storage-machine vsmc) (list lan dmz))
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
  (- (+ (reduce (fn [cost ^Machine machine] (+ cost (calculate-machine-cost machine))) the-storage-machines)
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
      (reduce (fn [[cheapest-cost-mc :as cheapest] mc] (let [cost-mc (calculate-machine-configuration-cost mc)]
                                                         (if (< cost-mc cheapest-cost-mc)
                                                           (list cost-mc mc)
                                                           cheapest)))
              all-valid-machine-configurations))))

(defn- find-cheapest-storage-system-for-this-storage-machine-configuration [smc]
  (let [storage-configuration-pattern (generate-storage-configuration-pattern smc)
        cheapest-storage-configuration (find-cheapest-storage-configuration-memo storage-configuration-pattern)

        cheapest-machine-configuration (find-cheapest-machine-configuration smc)]
    {:storage-configuration      cheapest-storage-configuration
     :storage-configuration-cost (calculate-storage-configuration-cost cheapest-storage-configuration
                                                                       storage-configuration-pattern)
     :machine-configuration      cheapest-machine-configuration
     :machine-configuration-cost (calculate-machine-configuration-cost cheapest-machine-configuration)}))

(defn- total-number-drives-in-storage-configuration [sc]
  (reduce (fn [r dac] (+ r (total-number-drives-in-dac dac))) sc))

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
                                        storage-systems)]
    (println (str "Cheapest system at " level " level costs " (+ (:storage-configuration-cost cheapest-storage-system)
                                                                 (:machine-configuration-cost cheapest-storage-system))))
    cheapest-storage-system))

(defn- find-the-cheapest-system-for-this-storage-machine-configuration-list [idx smcl]
  (let [cheapest-storage-systems (filter identity (map find-cheapest-storage-system-for-this-storage-machine-configuration smcl))]
    (when (seq cheapest-storage-systems)
      (find-cheapest-storage-system "smcl" cheapest-storage-systems))))

(defn- find-the-cheapest-system []
  (let [cheapest-storage-systems (filter identity (map find-the-cheapest-system-for-this-storage-machine-configuration-list
                                                       list-of-all-storage-machine-configuration-lists))]
    (when (seq cheapest-storage-systems)
      (find-cheapest-storage-system "lasmcl" cheapest-storage-systems))))


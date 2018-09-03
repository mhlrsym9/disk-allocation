(ns disk-allocation.third-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo])
  (:import (disk_allocation.data Machine)))

; dac stands for drive array configuration

(defn- extract-valid-drive-arrays [{:keys [all-drive-arrays]} number-drives-needed]
  (filter (fn [[{:keys [number-drives]}]]
            (= 0 (mod number-drives-needed number-drives)))
          all-drive-arrays))

(defn- create-all-dacs-of-size-from-valid-drive-arrays [number-drives-needed valid-drive-arrays]
  (mapcat (fn [[{:keys [number-drives]} :as das]]
            (distinct (map (fn [dac] (sort-by :tib-50-percent dac))
                           (combo/selections das (/ number-drives-needed number-drives)))))
          valid-drive-arrays))

(defn- create-all-valid-vm-dac-sizes [number-drives {:keys [target-size]}]
  (filter #(= number-drives (apply + %))
          (apply combo/cartesian-product
                 (repeat (count target-size)
                         (range 2 (inc number-drives))))))

(defn- create-all-dacs-of-size [scp-for-one-component number-drives-needed]
  (let [vdas (extract-valid-drive-arrays scp-for-one-component number-drives-needed)]
    (create-all-dacs-of-size-from-valid-drive-arrays number-drives-needed vdas)))

(defn- create-all-vm-dacs-of-size [number-drives scp-for-one-component]
  (let [all-drive-blocks (create-all-valid-vm-dac-sizes number-drives scp-for-one-component)]
    (mapcat (fn [drive-block] (apply combo/cartesian-product
                                     (map #(create-all-dacs-of-size scp-for-one-component %)
                                          drive-block)))
            all-drive-blocks)))

(defn- create-all-vm-dacs [{:keys [max-number-drives target-size] :as scp-for-one-component}]
  (mapcat (fn [number-drives]
            (create-all-vm-dacs-of-size number-drives scp-for-one-component))
          (range (* 2 (count target-size))
                 (inc max-number-drives))))

(defn- is-vm-dac-right? [{:keys [target-size] :as scp-for-one-component} percent-key vm-dac]
  (and (let [args (map list target-size vm-dac)]
         (every? #(utils/is-dac-right-size? (first %) (second %) percent-key) args))
       (utils/does-dac-have-right-physical-drive-size-configuration? scp-for-one-component
                                                                     (apply concat vm-dac))))

(defn- create-all-valid-vm-dacs [scp-for-one-component percent-key]
  (filter (partial is-vm-dac-right? scp-for-one-component percent-key)
          (create-all-vm-dacs scp-for-one-component)))

(defn- dac-cost [dac]
  (reduce (fn [r {:keys [number-drives], {:keys [drive-cost]} :drive}]
            (+ r (* number-drives drive-cost)))
          0.00M
          dac))

(defn- vm-dac-cost [vm-dac]
  (reduce (fn [r dac] (+ r (dac-cost dac))) 0.00M vm-dac))

(defn- calculate-drive-adjustment-with-vm-dacs [sc scp]
  (utils/calculate-drive-adjustment (map (fn [vm-dac] (apply concat vm-dac)) sc) scp))

(defn- calculate-storage-configuration-cost [sc scp]
  (when sc
    (- (reduce (fn [r dac] (+ r (vm-dac-cost dac))) 0.00M sc)
       (calculate-drive-adjustment-with-vm-dacs sc scp))))

(defn- total-number-drives-in-vm-dac [vm-dac]
  (reduce (fn [r dac] (+ r (utils/total-number-drives-in-dac dac))) 0 vm-dac))

(defn- total-number-drives-in-storage-configuration [sc]
  (reduce (fn [r vm-dac] (+ r (total-number-drives-in-vm-dac vm-dac))) 0 sc))

(defn- find-the-cheapest-storage-configuration [scp]
  (let [all-storage-configurations (apply combo/cartesian-product
                                          (map (fn [scp-for-one-component]
                                                 (create-all-valid-vm-dacs
                                                   scp-for-one-component
                                                   :tib-50-percent))
                                               scp))]
    (when (seq all-storage-configurations)
      (let [csc (reduce (fn [[cheapest-cost-sc cheapest-sc :as cheapest] sc]
                          (let [cost-sc (calculate-storage-configuration-cost sc scp)]
                            (cond (< cost-sc cheapest-cost-sc) (list cost-sc sc)
                                  (< cheapest-cost-sc cost-sc) cheapest
                                  :else (let [n-cheapest-sc (total-number-drives-in-storage-configuration cheapest-sc)
                                              n-sc (total-number-drives-in-storage-configuration sc)]
                                          (if (< n-sc n-cheapest-sc)
                                            (list cost-sc sc)
                                            cheapest)))))
                        (list (calculate-storage-configuration-cost (first all-storage-configurations) scp)
                              (first all-storage-configurations))
                        (rest all-storage-configurations))]
        (println (str "Cheapest storage configuration found costs " (first csc)))
        (second csc)))))

(def find-the-cheapest-storage-configuration-memo (memoize (fn [scp]
                                                             (let [answer (promise)]
                                                               (future (deliver answer
                                                                                (find-the-cheapest-storage-configuration
                                                                                  scp)))
                                                               answer))))

(def lock [])
(defn find-cheapest-storage-configuration [scp]
  (locking lock
    (find-the-cheapest-storage-configuration-memo scp)))

(defn- extract-machines-of-type [type machines]
  (filter #(= type (:type %)) machines))

(defn- extract-sorted-machine-names [machines]
  (sort (map #(get-in % [:case :name]) machines)))

(defn- good-case-configuration? [machine-names]
  (cond
    (= 0 (count machine-names)) true
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
  (let [all-machines (remove #(= :placeholder (:type %)) (concat smc (list lan dmz)))
        lan-machine-names (extract-sorted-machine-names (extract-machines-of-type :lan all-machines))
        dmz-machine-names (extract-sorted-machine-names (extract-machines-of-type :dmz all-machines))
        lan-and-dmz-machine-names (extract-sorted-machine-names (extract-machines-of-type :lan-and-dmz all-machines))]
    (and (good-case-configuration? lan-machine-names)
         (good-case-configuration? dmz-machine-names)
         (or (good-case-configuration? (sort (concat lan-machine-names lan-and-dmz-machine-names)))
             (good-case-configuration? (sort (concat dmz-machine-names lan-and-dmz-machine-names)))))))

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

(defn- count-matching-items-in-machine-configuration [[the-storage-machines & machines] item does-item-match-fnc?]
  (count (filter (fn [^Machine machine] (does-item-match-fnc? machine item))
                 (concat the-storage-machines machines))))

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

(defn- calculate-machine-configuration-cost [[the-storage-machines & machines :as mc]]
  (when mc
    (let [all-machines (concat the-storage-machines machines)]
      (- (reduce (fn [cost ^Machine machine] (+ cost (calculate-machine-cost machine))) 0.00M all-machines)
         (apply + (map (fn [[item match-fnc held]]
                         (adjust-cost-for-matching-items-in-system-configuration mc item match-fnc held))
                       adjustments))
         (let [number-msi (count-matching-items-in-machine-configuration mc msi-x99a-tomahawk does-mb-match?)
               number-augmented-msi (count-matching-items-in-machine-configuration mc augmented-msi-x99a-tomahawk does-mb-match?)]
           (if (or (< 0 number-msi)
                   (< 0 number-augmented-msi))
             (:cost msi-x99a-tomahawk)
             0.00M))))))

(defn- extract-total-three-point-five-drives [[the-storage-machines & machines]]
  (let [all-machines (concat the-storage-machines machines)]
    (reduce (fn [r v] (+ r (:three-point-five-drives (:case v)))) 0 all-machines)))

(defn- find-cheapest-machine-configuration [{:keys [lan-pool dmz-pool]} smc]
  (let [all-machine-configurations (combo/cartesian-product (list smc) lan-pool dmz-pool)
        all-valid-machine-configurations (filter keep-good-case-configurations all-machine-configurations)]
    (when (seq all-valid-machine-configurations)
      (second (reduce (fn [[cheapest-cost-mc cheapest-mc :as cheapest] mc]
                        (let [cost-mc (calculate-machine-configuration-cost mc)]
                          (cond (< cost-mc cheapest-cost-mc) (list cost-mc mc)
                                (< cheapest-cost-mc cost-mc) cheapest
                                :else (let [t-mc (extract-total-three-point-five-drives mc)
                                            t-cheapest-mc (extract-total-three-point-five-drives cheapest-mc)]
                                        (if (< t-mc t-cheapest-mc)
                                          (list cost-mc mc)
                                          cheapest)))))
                      (list (calculate-machine-configuration-cost (first all-valid-machine-configurations))
                            (first all-valid-machine-configurations))
                      (rest all-valid-machine-configurations))))))

(defn- remove-all-drive-arrays-from-machine [^Machine mc]
  (assoc mc :all-drive-arrays (list nil)))

(defn- remove-all-drive-arrays-from-machine-configuration [[smc lan dmz]]
  (list (map remove-all-drive-arrays-from-machine smc)
        (remove-all-drive-arrays-from-machine lan)
        (remove-all-drive-arrays-from-machine dmz)))

(defn- total-number-of-drives-in-machine [[s m]]
  (+ (total-number-drives-in-vm-dac s)
     (:three-point-five-drives-required (:rd m))
     (:two-point-five-drives-required (:rd m))))

(defn- final-cost-adjustment-for-xl-machines [storage-configuration [machine-configuration]]
  (let [paired-configurations (map #(list %1 %2) storage-configuration machine-configuration)
        xl-machines (filter (fn [[_ m]] (= (:name one-xl) (:name (:case m)))) paired-configurations)
        big-storage-xls (filter (fn [c] (= 13 (total-number-of-drives-in-machine c))) xl-machines)
        really-big-storage-xls (filter (fn [c] (= 14 (total-number-of-drives-in-machine c))) xl-machines)]
    (+ (if (seq really-big-storage-xls)
         (* 117.98M (count really-big-storage-xls))
         0.00M)
       (if (seq big-storage-xls)
         (* 58.99M (count big-storage-xls))
         0.00M))))

(defn- final-cost-adjustment-for-r5-machines [storage-configuration [machine-configuration]]
  (let [paired-configurations (map #(list %1 %2) storage-configuration machine-configuration)
        r5-machines (filter (fn [[_ m]] (= (:name one-r5) (:name (:case m)))) paired-configurations)
        big-storage-r5s (filter (fn [[s _ :as c]]
                                  (or (< 12 (total-number-of-drives-in-machine c))
                                      (and (< 10 (total-number-of-drives-in-machine c))
                                           (seq (filter #(= (:drive-size one-tb-drive) %)
                                                        (map :drive-size
                                                             (map :drive
                                                                  (apply concat
                                                                         (apply concat s)))))))))
                                r5-machines)]
    (if (seq big-storage-r5s)
      (* 58.99M (count big-storage-r5s))
      0.00M)))

(defn- final-cost-adjustment [storage-configuration machine-configuration]
  (apply + ((juxt final-cost-adjustment-for-xl-machines final-cost-adjustment-for-r5-machines)
             storage-configuration machine-configuration)))

(comment (defn- find-cheapest-storage-system-for-this-storage-machine-configuration [pool smc]
           (let [storage-configuration-pattern (utils/generate-storage-machine-configuration-pattern-v3 smc)
                 cheapest-storage-configuration (find-cheapest-storage-configuration
                                                  storage-configuration-pattern)
                 storage-configuration-cost (calculate-storage-configuration-cost
                                              @cheapest-storage-configuration
                                              storage-configuration-pattern)
                 cheapest-machine-configuration (find-cheapest-machine-configuration pool smc)
                 machine-configuration-cost (calculate-machine-configuration-cost
                                              cheapest-machine-configuration)]
             (if (and machine-configuration-cost storage-configuration-cost)
               (do
                 (println (str "Cheapest cost of this smc is "
                               (+ machine-configuration-cost storage-configuration-cost)))
                 {:storage-configuration      @cheapest-storage-configuration
                  :storage-configuration-cost storage-configuration-cost
                  :machine-configuration      (remove-all-drive-arrays-from-machine-configuration
                                                cheapest-machine-configuration)
                  :machine-configuration-cost machine-configuration-cost
                  :total-configuration-cost (+ storage-configuration-cost
                                               machine-configuration-cost
                                               (final-cost-adjustment csc cheapest-machine-configuration))})
               (println "No valid configuration for this smc!")))))

(defn- find-cheapest-storage-system-for-this-storage-machine-configuration [scp csc pool smc]
  (let [cheapest-machine-configuration (find-cheapest-machine-configuration pool smc)
        machine-configuration-cost (calculate-machine-configuration-cost cheapest-machine-configuration)
        storage-configuration-cost (calculate-storage-configuration-cost csc scp)]
    (if (and machine-configuration-cost storage-configuration-cost)
      (do
        (println (str "Cheapest cost of this smc is "
                      (+ storage-configuration-cost machine-configuration-cost)))
        {:storage-configuration      csc
         :storage-configuration-cost storage-configuration-cost
         :machine-configuration      (remove-all-drive-arrays-from-machine-configuration
                                       cheapest-machine-configuration)
         :machine-configuration-cost machine-configuration-cost
         :total-configuration-cost (+ storage-configuration-cost
                                      machine-configuration-cost
                                      (final-cost-adjustment csc cheapest-machine-configuration))})
      (println "No valid configuration for this smc!"))))

(defn- lowest-machine-count
  [{r-mc :machine-configuration :as r}
   {v-mc :machine-configuration :as v}]
  (let [r-machine-count (count r-mc)
        v-machine-count (count v-mc)]
    (cond (< v-machine-count r-machine-count) v
          (< r-machine-count v-machine-count) r
          :else nil)))

(defn- lowest-number-drives
  [{r-sc :storage-configuration :as r}
   {v-sc :storage-configuration :as v}]
  (let [r-drive-count (total-number-drives-in-storage-configuration r-sc)
        v-drive-count (total-number-drives-in-storage-configuration v-sc)]
    (cond (< v-drive-count r-drive-count) v
          (< r-drive-count v-drive-count) r
          :else nil)))

(defn- smallest-case
  [{r-mc :machine-configuration :as r}
   {v-mc :machine-configuration :as v}]
  (let [r-case-drives (extract-total-three-point-five-drives r-mc)
        v-case-drives (extract-total-three-point-five-drives v-mc)]
    (cond (< v-case-drives r-case-drives) v
          (< r-case-drives v-case-drives) r
          :else nil)))

(defn- use-other-heuristics-to-choose-best-storage-system [r v]
  (or (lowest-machine-count r v)
      (lowest-number-drives r v)
      (smallest-case r v)
      r))

(defn- find-cheapest-storage-system [level storage-systems]
  (let [cheapest-storage-system (reduce (fn [{r-total-cost :total-configuration-cost, :as r}
                                             {v-total-cost :total-configuration-cost, :as v}]
                                          (cond
                                            (< v-total-cost r-total-cost) v
                                            (= v-total-cost r-total-cost) (use-other-heuristics-to-choose-best-storage-system r v)
                                            :else r))
                                        (first storage-systems)
                                        (rest storage-systems))]
    (println (str "Cheapest system at "
                  level
                  " level costs "
                  (:total-configuration-cost cheapest-storage-system)))
    cheapest-storage-system))

(comment (defn- find-the-cheapest-system-for-this-storage-machine-configuration-list [{:keys [smc-pool] :as pool}]
           (let [cheapest-fnc (partial find-cheapest-storage-system-for-this-storage-machine-configuration pool)
                 futures-list (doall (map #(future (cheapest-fnc %)) smc-pool))
                 cheapest-storage-systems (filter identity (map deref futures-list))]
             (when (seq cheapest-storage-systems)
               (find-cheapest-storage-system "smcl" cheapest-storage-systems)))))

(defn- find-the-cheapest-system-for-this-storage-machine-configuration-list [{:keys [smc-pool] :as pool}]
  (let [all-needed-storage-configuration-patterns (apply hash-set
                                                         (map utils/generate-storage-machine-configuration-pattern-v3
                                                              smc-pool))
        futures-list (doall (map (fn [scp] (future (find-the-cheapest-storage-configuration scp)))
                                 all-needed-storage-configuration-patterns))
        cheapest-storage-configurations (into (hash-map)
                                              (filter (fn [[_ v]] v)
                                                      (map (fn [scp f] (vector scp (deref f)))
                                                           all-needed-storage-configuration-patterns
                                                           futures-list)))
        cheapest-fnc (fn [smc] (let [scp (utils/generate-storage-machine-configuration-pattern-v3 smc)
                                     csc (get cheapest-storage-configurations scp)]
                                 (find-cheapest-storage-system-for-this-storage-machine-configuration scp csc pool smc)))
        cheapest-storage-systems (filter identity (map cheapest-fnc smc-pool))]
    (when (seq cheapest-storage-systems)
      (find-cheapest-storage-system "smcl" cheapest-storage-systems))))

(defn- find-the-cheapest-system []
  (let [cheapest-storage-systems (filter identity (map find-the-cheapest-system-for-this-storage-machine-configuration-list
                                                       list-of-all-storage-machine-configuration-lists))]
    (when (seq cheapest-storage-systems)
      (find-cheapest-storage-system "lasmcl" cheapest-storage-systems))))


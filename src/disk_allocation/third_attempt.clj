(ns disk-allocation.third-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.core.memoize :as m])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.string :as str])
  (:require [zprint.core :as zp])
  (:import (disk_allocation.data Machine)))

; dac stands for drive array configuration

; Create a list of list of numbers with (count target-size) elements adding up to the
; number-drives specified. For example, with number-drives 10 and target-size w/ 2 elements, returns
; ((2 8) (3 7) (4 6) (5 5) (6 4) (7 3) (8 2))
; In this case, (2 8) and (8 2) are different because the first number refers to the first
; distinct vm, and the second the second one...
(defn- create-all-valid-vm-dac-drive-block-combinations [{:keys [target-size]} number-drives]
  (filter #(= number-drives (apply + %))
          (apply combo/cartesian-product
                 (repeat (count target-size)
                         (range 2 (inc number-drives))))))

(defn- create-all-drive-block-combinations [{:keys [max-number-drives target-size] :as scp-for-one-component}]
  (mapcat (partial create-all-valid-vm-dac-drive-block-combinations scp-for-one-component)
          (range (* 2 (count target-size)) (inc max-number-drives))))

; valid drive arrays are drive arrays whose size evenly divides into the number of drives needed.
(defn- extract-valid-drive-arrays [all-drive-arrays number-drives-needed]
  (filter (fn [[{:keys [number-drives]}]]
            (= 0 (mod number-drives-needed number-drives)))
          all-drive-arrays))

; The distinct portion is to account for the fact that an array of 3TB + 4TB drives is
; the same as an array of 4TB + 3TB drives, all other aspects being equal (number of drives,
; type of array, etc.)
(defn- create-all-dacs-of-size-from-valid-drive-arrays
  [number-drives-needed valid-drive-arrays]
  (mapcat (fn [[{:keys [number-drives]} :as das]]
            (distinct (map (fn [dac] (sort-by :tib-50-percent dac))
                           (combo/selections das (/ number-drives-needed number-drives)))))
          valid-drive-arrays))

(defn- create-all-dacs-with-specified-number-of-drives [all-drive-arrays number-of-drives]
  (let [vdas (extract-valid-drive-arrays all-drive-arrays number-of-drives)]
    (create-all-dacs-of-size-from-valid-drive-arrays number-of-drives vdas)))

(def create-all-dacs-with-specified-number-of-drives-memo
  (memoize create-all-dacs-with-specified-number-of-drives))

(defn- create-all-vm-dacs-for-one-component [{:keys [all-drive-arrays]} drive-block]
  (apply combo/cartesian-product (map (partial create-all-dacs-with-specified-number-of-drives-memo
                                               all-drive-arrays)
                                      drive-block)))

(defn- is-vm-dac-right? [percent-key {:keys [target-size] :as scp-for-one-component} vm-dac]
  (and (let [args (map list target-size vm-dac)]
         (every? #(utils/is-dac-right-size? (first %) (second %) percent-key) args))
       (utils/does-dac-have-right-physical-drive-size-configuration? scp-for-one-component
                                                                     (apply concat vm-dac))))

(defn- dac-cost [dac]
  (reduce (fn [r {:keys [number-drives], {:keys [drive-cost]} :drive}]
            (+ r (* number-drives drive-cost)))
          0.00M
          dac))

(defn- filter-by-drives [vm-dac]
  (let [drives (map #(+ (utils/number-drives-in-dac one-tb-drive %)
                        (utils/number-drives-in-dac four-tb-drive %))
                    vm-dac)]
    (some #(not= 0 %) drives)))

(defn- vm-dac-cost [vm-dac]
  (reduce (fn [r dac] (+ r (dac-cost dac))) 0.00M vm-dac))

(defn- vm-dac-reducer [r v]
  (cond (nil? r) v
        (nil? v) r
        :else (let [r-cost (vm-dac-cost r)
                    v-cost (vm-dac-cost v)]
                (if (< v-cost r-cost) v r))))

(defn- vm-dac-combiner
  ([] nil)
  ([r v] (vm-dac-reducer r v)))

(defn- create-all-valid-vm-dacs-for-one-component [percent-key scp-for-one-component drive-block]
  (let [initial-vm-dacs (create-all-vm-dacs-for-one-component scp-for-one-component drive-block)
        valid-vm-dacs (r/filter (partial is-vm-dac-right?
                                         percent-key
                                         scp-for-one-component)
                                initial-vm-dacs)
        vm-dacs-with-one-or-four-tb-drives (r/foldcat (r/filter filter-by-drives valid-vm-dacs))
        cheapest-vm-dac (->> valid-vm-dacs
                             (r/remove (set vm-dacs-with-one-or-four-tb-drives))
                             (r/fold vm-dac-combiner vm-dac-reducer))
        final-vm-dacs (filter identity (cons cheapest-vm-dac vm-dacs-with-one-or-four-tb-drives))]
    final-vm-dacs))

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

(defn- generate-all-possible-storage-configurations [scp drive-block-combination]
  (map (partial create-all-valid-vm-dacs-for-one-component
                :tib-50-percent)
       scp drive-block-combination))

(defn- determine-cheaper-storage-configuration [{r-cost-sc :cheapest-cost-sc,
                                                 r-sc :cheapest-sc,
                                                 :as r}
                                                {v-cost-sc :cheapest-cost-sc,
                                                 v-sc :cheapest-sc,
                                                 :as v}]
  (cond (nil? r-cost-sc) v
        (nil? v-cost-sc) r
        (< v-cost-sc r-cost-sc) v
        (< r-cost-sc v-cost-sc) r
        :else (let [n-r-sc (total-number-drives-in-storage-configuration r-sc)
                    n-v-sc (total-number-drives-in-storage-configuration v-sc)]
                (if (< n-v-sc n-r-sc) v r))))

(defn- csc-reducer [scp cheapest-sc-so-far sc]
  (let [cost-sc (calculate-storage-configuration-cost sc scp)
        new-val {:cheapest-cost-sc cost-sc :cheapest-sc sc}]
    (determine-cheaper-storage-configuration cheapest-sc-so-far new-val)))

(defn- reduce-csc-pairs [pairs]
  (reduce determine-cheaper-storage-configuration
          {:cheapest-cost-sc nil :cheapest-sc nil}
          pairs))

(defn- csc-combiner
  ([] {:cheapest-cost-sc nil :cheapest-sc nil})
  ([& m] (reduce-csc-pairs m)))

(defn- unique-str-one-drive-array [itm]
  (str "(" (str/join " " (map #(:array-name %) itm)) ")"))

(defn- unique-str-one-dac [itm]
  (str "(" (str/join (map unique-str-one-drive-array itm)) ")"))

(defn- unique-str-all-possible-storage-configurations-for-one-component [idx itm]
  (str "(" idx "-" (apply str (map unique-str-one-dac itm)) ")"))

(defn- unique-str-all-possible-storage-configurations [p]
  (apply str (map-indexed unique-str-all-possible-storage-configurations-for-one-component p)))

(defn- ^{:clojure.core.memoize/args-fn (comp unique-str-all-possible-storage-configurations second)}
locate-cheapest-storage-configuration
  [scp all-possible-storage-configurations]
  (let [all-storage-configurations (apply combo/cartesian-product
                                          all-possible-storage-configurations)]
    (reduce-csc-pairs (map #(r/fold csc-combiner (partial csc-reducer scp) (vec %))
                           (partition-all 1000000 all-storage-configurations)))))

(def locate-cheapest-storage-configuration-memo
  (m/memo #'locate-cheapest-storage-configuration))

(defn- ^{:clojure.core.memoize/args-fn first}
find-the-cheapest-storage-configuration
  [scp smaller-scp]
  (let [all-drive-block-combinations (apply combo/cartesian-product
                                            (map create-all-drive-block-combinations scp))
        all-smaller-drive-block-combinations (apply combo/cartesian-product
                                                    (map create-all-drive-block-combinations smaller-scp))
        remaining-drive-block-combinations (apply disj
                                                  (set all-drive-block-combinations)
                                                  all-smaller-drive-block-combinations)
        csc-pair (->> remaining-drive-block-combinations
                      (r/map (partial generate-all-possible-storage-configurations scp))
                      (r/remove (partial some empty?))
                      (r/map (partial locate-cheapest-storage-configuration-memo scp))
                      (r/fold csc-combiner))]
    (if (:cheapest-cost-sc csc-pair)
      (println (str "Cheapest storage configuration found costs " (:cheapest-cost-sc csc-pair)))
      (println "No configuration found!"))
    (:cheapest-sc csc-pair)))

(def find-the-cheapest-storage-configuration-memo (m/memo #'find-the-cheapest-storage-configuration))

(defn find-cheapest-storage-configuration
  ([scp] (find-cheapest-storage-configuration scp nil))
  ([scp smaller-scp]
   (find-the-cheapest-storage-configuration-memo scp smaller-scp)))

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
      (- (reduce (fn [cost ^Machine machine]
                   (+ cost (calculate-machine-cost machine)))
                 0.00M
                 all-machines)
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

(defn- find-cheapest-machine-configuration [{:keys [storage-farm-configuration lan-pool dmz-pool]}]
  (let [all-machine-configurations (combo/cartesian-product (list storage-farm-configuration)
                                                            lan-pool
                                                            dmz-pool)
        all-valid-machine-configurations (filter keep-good-case-configurations
                                                 all-machine-configurations)]
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

(defmulti find-the-cheapest-farm :configuration)

(defmethod find-the-cheapest-farm :actual-farms [{:keys [level actual-farms]}]
  (let [cheapest-farm (reduce (fn [{r-total-cost :total-configuration-cost, :as r}
                                   {v-total-cost :total-configuration-cost, :as v}]
                                (cond
                                  (< v-total-cost r-total-cost) v
                                  (= v-total-cost r-total-cost) (use-other-heuristics-to-choose-best-storage-system r v)
                                  :else r))
                              (first actual-farms)
                              (rest actual-farms))]
    (println (str "Cheapest system at "
                  level
                  " level costs "
                  (:total-configuration-cost cheapest-farm)))
    cheapest-farm))

(defmethod find-the-cheapest-farm :single-storage-farm-configuration-pool
  [{:keys [storage-farm-configuration-pattern
           previous-storage-farm-configuration-pattern
           name] :as pool}]
  (let [cheapest-storage-configuration (find-cheapest-storage-configuration
                                         storage-farm-configuration-pattern
                                         previous-storage-farm-configuration-pattern)
        storage-configuration-cost (calculate-storage-configuration-cost
                                     cheapest-storage-configuration
                                     storage-farm-configuration-pattern)
        cheapest-machine-configuration (find-cheapest-machine-configuration pool)
        machine-configuration-cost (calculate-machine-configuration-cost
                                     cheapest-machine-configuration)]
    (when (and machine-configuration-cost storage-configuration-cost)
      (let [final-cost-adjustment (final-cost-adjustment cheapest-storage-configuration
                                                         cheapest-machine-configuration)]
        {:name                       name
         :storage-configuration      cheapest-storage-configuration
         :storage-configuration-cost storage-configuration-cost
         :machine-configuration      (remove-all-drive-arrays-from-machine-configuration
                                       cheapest-machine-configuration)
         :machine-configuration-cost machine-configuration-cost
         :final-cost-adjustment      final-cost-adjustment
         :total-configuration-cost   (+ storage-configuration-cost
                                        machine-configuration-cost
                                        final-cost-adjustment)}))))

(defn- adjust-storage-farm-configuration [pool sfc]
  (assoc (dissoc pool :storage-farm-configuration-pool)
    :storage-farm-configuration sfc
    :configuration :single-storage-farm-configuration-pool
    :storage-farm-configuration-pattern (utils/generate-storage-farm-configuration-pattern-v3 sfc)))

(defn- by-two-point-five-drives-max-number-drives [l r]
  (compare [(:number-two-point-five-drives l) (:max-number-drives l)]
           [(:number-two-point-five-drives r) (:max-number-drives r)]))

(defn- by-storage-farm-configuration-pattern [{l :storage-farm-configuration-pattern}
                                              {r :storage-farm-configuration-pattern}]
  (let [c (some (fn [[il ir]]
                  (let [c (by-two-point-five-drives-max-number-drives il ir)]
                    (when (not= 0 c) c)))
                (map (fn [il ir] (list il ir)) l r))]
    (if c
      c
      0)))

(defn- extract-number-two-point-five-drives [{:keys [storage-farm-configuration-pattern]}]
  (map #(:number-two-point-five-drives %) storage-farm-configuration-pattern))

(defn- assoc-previous-storage-farm-configuration-pattern [[f s]]
  (assoc f :previous-storage-farm-configuration-pattern (:storage-farm-configuration-pattern s)))

(defn- add-previous-storage-farm-configuration-pattern [storage-farm-configuration-strip]
  (let [start-strip (assoc-previous-storage-farm-configuration-pattern
                      (list (first storage-farm-configuration-strip)
                            nil))]
    (if (= 1 (count storage-farm-configuration-strip))
      (list start-strip)
      (cons start-strip (map assoc-previous-storage-farm-configuration-pattern
                             (map reverse (partition 2 1 storage-farm-configuration-strip)))))))

(defmethod find-the-cheapest-farm :farm-configuration-pool [{:keys [storage-farm-configuration-pool] :as pool}]
  (let [cheapest-farms (->> storage-farm-configuration-pool
                            (map (partial adjust-storage-farm-configuration pool))
                            (sort by-storage-farm-configuration-pattern)
                            (partition-by extract-number-two-point-five-drives)
                            vec
                            (r/map (partial vec))
                            (r/mapcat add-previous-storage-farm-configuration-pattern)
                            (r/map find-the-cheapest-farm)
                            (r/filter identity)
                            (into (list)))]
    (when (seq cheapest-farms)
      (find-the-cheapest-farm {:configuration :actual-farms
                               :level "farm-configuration-pool"
                               :actual-farms cheapest-farms}))))

(defmethod find-the-cheapest-farm :all-farm-configuration-pools [{:keys [all-farm-configuration-pools]}]
  (when-let [cheapest-farms (filter identity
                                    (map find-the-cheapest-farm
                                         (take 2 all-farm-configuration-pools)))]
    (let [cheapest-farm (find-the-cheapest-farm {:configuration :actual-farms
                                                 :level "all-farm-configuration-pools"
                                                 :actual-farms cheapest-farms})]
      (zp/zprint cheapest-farm)
      cheapest-farm)))
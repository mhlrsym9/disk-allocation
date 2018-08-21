(ns disk-allocation.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defrecord Cpu [cost name])
(def e5-2603-v3 (->Cpu 221.00M "e5-2603-v3"))
(def e5-2603-v4 (->Cpu 229.99M "e5-2603-v4"))
(def g3900 (->Cpu 36.99M "g3900"))
(def g3930 (->Cpu 35.99M "g3930"))
(def atom-c2750 (->Cpu 0.01M "atom-c2750") )

(defrecord Hba [additional-sata-connectors cost name])
(def hba-9211-4i (->Hba 4 75.50M "9211-4i"))
(def hba-9211-8i (->Hba 8 78.00M "9211-8i"))
(def hba-none (->Hba 0 0.00M "no hba"))

(defrecord Motherboard [number-sata-connections cost])
(def msi-x99a-tomahawk (->Motherboard 10 (+ 104.99M 235.99M))) ; 16GB ECC memory
(def augmented-msi-x99a-tomahawk (->Motherboard 10 (+ 104.99M 235.99M 235.99M))) ; 32GB ECC memory
(def asrock-x99m (->Motherboard 10 (+ 199.99M 53.11M 53.11M))) ; DeepServer3
(def ga-9sisl (->Motherboard 6 (- (+ 233.99M 98.00M) (:cost atom-c2750)))) ; DeepDivide
(def supermicro-x11ssh (->Motherboard 8 (+ 198.99M 117.99M))) ; Motherboard plus 8GB ECC memory
(def augmented-supermicro-x11ssh (->Motherboard 8 (+ 198.99M 235.99M))) ; Motherboard plus 16GB ECC memory

(defrecord Case [three-point-five-drives two-point-five-drives cost name])
(def one-r5 (->Case 11 2 123.80M "r5"))
(def one-xl (->Case 14 0 132.99M "xl"))
(def one-phanteks-itx (->Case 2 3 89.99M "Phanteks ITX"))
(def one-define-mini (->Case 9 0 (* 2 (+ 123.52M 39.97M)) "Mini"))
(def one-silencio (->Case 4 4 79.99M "Silencio"))

(def one-tb-to-tib 0.909495M)
(def lan-server-target-size (* 9.128M 2.5M one-tb-to-tib))
(def lan-client-target-size (* 5.812M 1.5M one-tb-to-tib))
(def lan-combined-target-size (+ lan-server-target-size lan-client-target-size))

(def dmz-server-target-size (* 1.3M 2.5M one-tb-to-tib))
(def dmz-client-target-size (* 0.48M 1.5M one-tb-to-tib))
(def dmz-combined-target-size (+ dmz-server-target-size dmz-client-target-size))

(defrecord Machine [^Case case ^Motherboard mb ^Cpu cpu ^Hba hba target-size])

(defn- generate-machines [all-components]
  (map (fn [[case mb cpu hba size]] (->Machine case mb cpu hba size)) all-components))

(def all-lan-servers (generate-machines (combo/cartesian-product (list one-r5 one-xl one-define-mini one-silencio)
                                                                 (list asrock-x99m)
                                                                 (list e5-2603-v4)
                                                                 (list hba-none)
                                                                 (list nil))))

(def all-dmz-servers (generate-machines (combo/cartesian-product (list one-r5 one-xl one-define-mini one-silencio one-phanteks-itx)
                                                                 (list ga-9sisl)
                                                                 (list atom-c2750)
                                                                 (list hba-none)
                                                                 (list nil))))

(def all-storage-in-one-machine
  (map list
       (generate-machines (combo/cartesian-product (list one-r5)
                                                   (list augmented-msi-x99a-tomahawk)
                                                   (list e5-2603-v3)
                                                   (list nil hba-9211-4i hba-9211-8i)
                                                   (list (list lan-combined-target-size dmz-combined-target-size))))))

(defn- big-storage-box [size-list]
  (generate-machines (combo/cartesian-product (list one-r5 one-xl)
                                              (list msi-x99a-tomahawk)
                                              (list e5-2603-v3)
                                              (list hba-none hba-9211-4i hba-9211-8i)
                                              size-list)))

(defn- small-storage-box [size-list]
  (generate-machines (combo/cartesian-product (list one-r5 one-xl one-silencio one-define-mini)
                                              (list supermicro-x11ssh)
                                              (list g3900 g3930)
                                              (list hba-none hba-9211-4i hba-9211-8i)
                                              size-list)))

(def all-storage-in-two-machines
  (combo/cartesian-product
       (big-storage-box (list (list lan-combined-target-size)))
       (small-storage-box (list (list dmz-combined-target-size)))))

(def storage-with-lan-split
  (combo/cartesian-product
       (big-storage-box (list (list lan-server-target-size)))
       (small-storage-box (list (list lan-client-target-size)))
       (small-storage-box (list (list dmz-combined-target-size)))))

(def storage-with-dmz-split
  (combo/cartesian-product
       (big-storage-box (list (list lan-combined-target-size)))
       (small-storage-box (list (list dmz-server-target-size)))
       (small-storage-box (list (list dmz-client-target-size)))))

(def all-storage-in-four-machines
  (combo/cartesian-product
       (big-storage-box (list (list lan-server-target-size)))
       (small-storage-box (list (list lan-client-target-size)))
       (small-storage-box (list (list dmz-server-target-size)))
       (small-storage-box (list (list dmz-client-target-size)))))

(def all-storage-machine-configurations (concat all-storage-in-two-machines
                                                storage-with-lan-split
                                                storage-with-dmz-split
                                                all-storage-in-four-machines))

(defrecord Drive [drive-size can-be-two-point-five-drive drive-cost])
(def one-tb-drive (Drive. 1 true 59.99M))
(def four-tb-drive (Drive. 4 false 117.99M))                ; NewEgg 2018-08-20 flyer, ends 2018-08-22
(def six-tb-drive (Drive. 6 false 174.99M))                 ; NewEgg 2018-08-17 flyer, ends 2018-08-21
(def eight-tb-drive (Drive. 8 false 226.70M))
(def ten-tb-drive (Drive. 10 false 304.48M))
(def twelve-tb-drive (Drive. 12 false 399.89M))

(defrecord DriveArray [array-name number-drives tib-50-percent tib-80-percent drive])

(def one-tb-raid-one-z-three-drive-array (DriveArray. "1TB RAID-1Z" 3 0.85 1.39 one-tb-drive))
(def four-tb-raid-one-z-three-drive-array (DriveArray. "4TB RAID-1Z" 3 3.40 5.57 four-tb-drive))
(def six-tb-raid-one-z-three-drive-array (DriveArray. "6TB RAID-1Z" 3 5.08 8.33 six-tb-drive))
(def eight-tb-raid-one-z-three-drive-array (DriveArray. "8TB RAID-1Z" 3 6.79 11.15 eight-tb-drive))
(def ten-tb-raid-one-z-three-drive-array (DriveArray. "10TB RAID-1Z" 3 8.51 13.97 ten-tb-drive))
(def twelve-tb-raid-one-z-three-drive-array (DriveArray. "12TB RAID-1Z" 3 10.16 16.66 twelve-tb-drive))
(def raid-one-z-three-drive-arrays (list one-tb-raid-one-z-three-drive-array
                                         four-tb-raid-one-z-three-drive-array
                                         six-tb-raid-one-z-three-drive-array
                                         eight-tb-raid-one-z-three-drive-array
                                         ten-tb-raid-one-z-three-drive-array
                                         twelve-tb-raid-one-z-three-drive-array))

(def one-tb-raid-one-z-four-drive-array (DriveArray. "1TB RAID-1Z" 4 1.24 2.03 one-tb-drive))
(def four-tb-raid-one-z-four-drive-array (DriveArray. "4TB RAID-1Z" 4 4.94 8.11 four-tb-drive))
(def six-tb-raid-one-z-four-drive-array (DriveArray. "6TB RAID-1Z" 4 7.41 12.16 six-tb-drive))
(def eight-tb-raid-one-z-four-drive-array (DriveArray. "8TB RAID-1Z" 4 9.89 16.21 eight-tb-drive))
(def ten-tb-raid-one-z-four-drive-array (DriveArray. "10TB RAID-1Z" 4 12.36 20.27 ten-tb-drive))
(def twelve-tb-raid-one-z-four-drive-array (DriveArray. "12TB RAID-1Z" 4 14.83 24.32 twelve-tb-drive))
(def raid-one-z-four-drive-arrays (list one-tb-raid-one-z-four-drive-array
                                        four-tb-raid-one-z-four-drive-array
                                        six-tb-raid-one-z-four-drive-array
                                        eight-tb-raid-one-z-four-drive-array
                                        ten-tb-raid-one-z-four-drive-array
                                        twelve-tb-raid-one-z-four-drive-array))

(def one-tb-raid-one-z-five-drive-array (DriveArray. "1TB RAID-1Z" 5 1.70 2.79 one-tb-drive))
(def four-tb-raid-one-z-five-drive-array (DriveArray. "4TB RAID-1Z" 5 6.80 11.15 four-tb-drive))
(def six-tb-raid-one-z-five-drive-array (DriveArray. "6TB RAID-1Z" 5 10.22 16.76 six-tb-drive))
(def eight-tb-raid-one-z-five-drive-array (DriveArray. "8TB RAID-1Z" 5 13.59 22.29 eight-tb-drive))
(def ten-tb-raid-one-z-five-drive-array (DriveArray. "10TB RAID-1Z" 5 16.97 27.83 ten-tb-drive))
(def twelve-tb-raid-one-z-five-drive-array (DriveArray. "12TB RAID-1Z" 5 20.44 33.52 twelve-tb-drive))
(def raid-one-z-five-drive-arrays (list one-tb-raid-one-z-five-drive-array
                                        four-tb-raid-one-z-five-drive-array
                                        six-tb-raid-one-z-five-drive-array
                                        eight-tb-raid-one-z-five-drive-array
                                        ten-tb-raid-one-z-five-drive-array
                                        twelve-tb-raid-one-z-five-drive-array))

(def one-tb-mirror-drive-array (DriveArray. "1TB MIRROR" 2 0.42 0.70 one-tb-drive))
(def four-tb-mirror-drive-array (DriveArray. "4TB MIRROR" 2 1.69 2.79 four-tb-drive))
(def six-tb-mirror-drive-array (DriveArray. "6TB MIRROR" 2 2.55 4.18 six-tb-drive))
(def eight-tb-mirror-drive-array (DriveArray. "8TB MIRROR" 2 3.40 5.57 eight-tb-drive))
(def ten-tb-mirror-drive-array (DriveArray. "10TB MIRROR" 2 4.25 6.97 ten-tb-drive))
(def twelve-tb-mirror-drive-array (DriveArray. "12TB MIRROR" 2 5.10 8.36 twelve-tb-drive))
(def mirror-drive-arrays (list one-tb-mirror-drive-array
                              four-tb-mirror-drive-array
                              six-tb-mirror-drive-array
                              eight-tb-mirror-drive-array
                              ten-tb-mirror-drive-array
                              twelve-tb-mirror-drive-array))

(def all-drive-arrays (list raid-one-z-three-drive-arrays
                            raid-one-z-four-drive-arrays
                            raid-one-z-five-drive-arrays
                            mirror-drive-arrays))

(defn- make-possibilities [number r]
  (map #(repeat % r) (range 0 number)))

(defn- is-right-physical-drive-size [case drive-arrays-configuration]
  (if (seq drive-arrays-configuration)
    (if (:can-be-two-point-five-drive (first drive-arrays-configuration))
      true
      (<= (* (:number-drives (first drive-arrays-configuration))
             (count drive-arrays-configuration))
          (:three-point-five-drives case)))
    true))

(defn- valid-machines [percent-key drive-arrays case target-size]
  (let [block-range (inc (int (Math/floor (/ (+ (:three-point-five-drives case)
                                                (:two-point-five-drives case))
                                             (:number-drives (first drive-arrays))))))
        all-drive-arrays-configurations (map flatten
                                            (apply combo/cartesian-product
                                                   (map #(make-possibilities block-range %)
                                                        drive-arrays)))
        configurations-with-right-number-of-drive-arrays (filter #(> block-range (count %))
                                                                 all-drive-arrays-configurations)
        configurations-with-right-physical-drive-sizes (filter (partial is-right-physical-drive-size case)
                                                               configurations-with-right-number-of-drive-arrays)
        configurations-with-right-target-size (filter #(< target-size (reduce (fn [r v] (+ r (percent-key v))) 0 %))
                                                      configurations-with-right-physical-drive-sizes)]
    (map (fn [v] {:valid-drive-array-configuration v :case case}) configurations-with-right-target-size)))

(defn- generate-all-valid-machines-in-system [[list-of-drive-arrays cases target-sizes]]
  (let [the-valid-configurations (map (fn [drive-arrays case target-size]
                                        (valid-machines :tib-50-percent
                                                        drive-arrays
                                                        case
                                                        target-size))
                                      list-of-drive-arrays cases target-sizes)]
    (apply combo/cartesian-product the-valid-configurations)))

(defn- drive-size [drive-array]
  (:drive-size (:drive drive-array)))

(defn- drive-cost [drive-array]
  (:drive-cost (:drive drive-array)))

(defn- cost-of-drive-array [drive-array]
  (* (drive-cost drive-array) (:number-drives drive-array)))

(defn- final-cost-of-configuration [{:keys [valid-drive-array-configuration case]}]
  (let [number-drives-in-array (:number-drives (first valid-drive-array-configuration))
        total-number-of-drives-in-configuration (* (count valid-drive-array-configuration)
                                                   number-drives-in-array)]
    {:the-cost            (reduce (fn [r v] (+ r (cost-of-drive-array v))) 0 valid-drive-array-configuration)
     :the-additional-cost (:cost case)
     :count-4tb           (* number-drives-in-array
                             (count (filter #(= (drive-size %)
                                                (:drive-size four-tb-drive))
                                            valid-drive-array-configuration)))
     :count-1tb           (* number-drives-in-array
                             (count (filter #(= (drive-size %)
                                                (:drive-size one-tb-drive))
                                            valid-drive-array-configuration)))}))

(defn- accumulate-from [fccs k]
  (reduce (fn [r v] (+ r (k v))) 0 fccs))

(defn- calculate-storage-system-cost [cs]
  (let [fccs (map final-cost-of-configuration cs)
        accumulate-from-fccs (partial accumulate-from fccs)]
    (- (+ (accumulate-from-fccs :the-cost)
          (accumulate-from-fccs :the-additional-cost))
       (+ (* (:drive-cost four-tb-drive) (min (accumulate-from-fccs :count-4tb) 9))
          (* (:drive-cost one-tb-drive) (min (accumulate-from-fccs :count-1tb) 4))))))

(defn- generate-valid-storage-systems [s]
  {:valid-storage-systems (list {:the-total-cost (calculate-storage-system-cost s)
                                 :machines       s})})

(defn- retrieve-the-total-cost [c]
  (if c
    (:the-total-cost (first (:valid-storage-systems c)))
    0.00M))

(defn- retrieve-the-total-drives [c]
  (if c
    (let [machines (:machines (first (:valid-storage-systems c)))]
      (apply + (map #(:number-drives (first (:valid-drive-array-configuration %))) machines)))
    0))

(defn- replace-if-cheaper [vss r]
  (let [the-total-cost-vss (retrieve-the-total-cost vss)
        the-total-cost-r (retrieve-the-total-cost r)]
    (cond
      (nil? r) vss
      (< the-total-cost-vss the-total-cost-r) vss
      (= the-total-cost-vss the-total-cost-r)
      (let [the-total-drives-vss (retrieve-the-total-drives vss)
            the-total-drives-r (retrieve-the-total-drives r)]
        (if (< the-total-drives-vss the-total-drives-r)
          vss
          r))
      :else r)))

(defn- generate-cheapest-valid-storage-systems [l]
  (let [result (reduce (fn [r v] (let [vss (generate-valid-storage-systems v)]
                                   (replace-if-cheaper vss r)))
                       nil (generate-all-valid-machines-in-system l))]
    (println (retrieve-the-total-cost result))
    result))

(defn- is-right-number-of-sata-connections [{:keys [number-sata-connections]} hba drive-array-configuration]
  (let [total-sata-connections (+ number-sata-connections (:additional-sata-connectors hba))
        total-number-drives (* (:number-drives (first drive-array-configuration)) (count drive-array-configuration))]
    (>= total-sata-connections total-number-drives)))

(defn- generate-all-valid-machines [{:keys [case mb hba], [the-target-size] :target-size} percent-key drive-arrays]
  (let [block-range (inc (int (Math/floor (/ (+ (:three-point-five-drives case)
                                                (:two-point-five-drives case))
                                             (:number-drives (first drive-arrays))))))
        all-drive-arrays-configurations (filter seq
                                                (map (fn [dac] (apply concat dac))
                                                     (apply combo/cartesian-product
                                                            (map (fn [da] (make-possibilities block-range da))
                                                                 drive-arrays))))
        configurations-with-right-number-of-drive-arrays (filter #(> block-range (count %))
                                                                 all-drive-arrays-configurations)
        configurations-with-right-physical-drive-sizes (filter (partial is-right-physical-drive-size case)
                                                               configurations-with-right-number-of-drive-arrays)
        configurations-with-right-number-of-sata-connections (filter (partial is-right-number-of-sata-connections mb hba)
                                                                     configurations-with-right-physical-drive-sizes)]
    (filter #(< the-target-size (reduce (fn [r v] (+ r (percent-key v))) 0 %))
            configurations-with-right-number-of-sata-connections)))

(defn- generate-all-valid-storage-machines [^Machine sm]
  (map (fn [dac] {:drive-array-configuration dac :storage-machine sm})
       (mapcat (fn [das] (generate-all-valid-machines sm :tib-50-percent das)) all-drive-arrays)))

(defn- generate-all-valid-storage-machine-configurations [smc]
  (apply combo/cartesian-product (map #(generate-all-valid-storage-machines %) smc)))

(defn- drive-array-total-cost [{:keys [number-drives], {:keys [drive-cost]} :drive}]
  (* number-drives drive-cost))

(defn- machine-total-cost [^Machine machine]
  (+ (:cost (:case machine))
     (:cost (:mb machine))
     (:cost (:cpu machine))
     (:cost (:hba machine))))

(defn- storage-machine-total-cost [{:keys [drive-array-configuration storage-machine]}]
  (+ (reduce (fn [r v] (+ r (drive-array-total-cost v))) 0.00M drive-array-configuration)
     (machine-total-cost storage-machine)))

(defn- storage-configuration-total-cost [[smc lan dmz]]
  (+ (reduce (fn [r v] (+ r (storage-machine-total-cost v))) 0.00M smc)
     (machine-total-cost lan)
     (machine-total-cost dmz)))

(defn- storage-configuration-machine-count [sc]
  2)

(defn- find-cheapest-system []
  (let [all-valid-storage-machine-configurations (mapcat #(generate-all-valid-storage-machine-configurations %)
                                                         all-storage-machine-configurations)
        all-valid-system-configurations (combo/cartesian-product all-valid-storage-machine-configurations
                                                                 all-lan-servers
                                                                 all-dmz-servers)]
    (reduce (fn [r v] (let [r-cost (storage-configuration-total-cost r)
                            v-cost (storage-configuration-total-cost v)]
                        (cond
                          (< v-cost r-cost) v
                          (= v-cost r-cost) (let [r-count (storage-configuration-machine-count r)
                                                  v-count (storage-configuration-machine-count v)]
                                              (if (< v-count r-count)
                                                v
                                                r))
                          :else r)))
            (first all-valid-system-configurations)
            (rest (take 10 all-valid-system-configurations)))))





(defn- configuration-to-drive-array-names [c]
  (map #(str (:array-name %) " x" (:number-drives %)) c))

(defn- output-configuration [{:keys [valid-drive-array-configuration case]}]
  (println "----")
  (println (:name case))
  (println (configuration-to-drive-array-names valid-drive-array-configuration)))

(defn -main []
  (let [small-cases (combo/selections (list one-phanteks-itx
                                            one-silencio) 1)
        combined (combo/cartesian-product (combo/selections (list raid-one-z-three-drive-arrays
                                                                  raid-one-z-four-drive-arrays
                                                                  raid-one-z-five-drive-arrays
                                                                  mirror-drive-arrays) 2)
                                          (concat (map #(concat (list one-r5) %) small-cases)
                                                  (map #(concat (list one-xl) %) small-cases))
                                          (list (list lan-combined-target-size dmz-combined-target-size)))
        separate (combo/cartesian-product (combo/selections (list raid-one-z-three-drive-arrays
                                                                  raid-one-z-four-drive-arrays
                                                                  raid-one-z-five-drive-arrays
                                                                  mirror-drive-arrays) 3)
                                          (concat (map #(concat (list one-r5 one-r5) %) small-cases)
                                                  (map #(concat (repeat 2 one-xl) %) small-cases))
                                          (list (list lan-server-target-size
                                                      lan-client-target-size
                                                      dmz-combined-target-size)))
        all-valid-storage-systems (apply merge-with
                                         concat
                                         (filter identity
                                                 (map generate-cheapest-valid-storage-systems
                                                      (concat combined separate))))
        final-answer (reduce (fn [r v] (let [vss {:valid-storage-systems (list v)}]
                                         (replace-if-cheaper vss r)))
                             nil
                             (:valid-storage-systems all-valid-storage-systems))
        cheapest-valid-storage-system (first (:valid-storage-systems final-answer))]
    (println "----")
    (println (:the-total-cost cheapest-valid-storage-system))
    (dorun (map #(output-configuration %) (:machines cheapest-valid-storage-system)))))



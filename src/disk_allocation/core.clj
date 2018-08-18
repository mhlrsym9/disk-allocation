(ns disk-allocation.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defrecord Case [drives additional-cost name])

(def one-r5 (Case. 10 0.00M "r5"))
(def one-xl (Case. 14 132.99M "xl"))
(def one-phanteks-itx (Case. 5 89.99M "Phanteks ITX"))

(def one-tb-to-tib 0.909495M)
(def lan-server-target-size (* 9.128M 2.5M one-tb-to-tib))
(def lan-client-target-size (* 5.812M 1.5M one-tb-to-tib))
(def lan-combined-target-size (+ lan-server-target-size lan-client-target-size))

(def dmz-server-target-size (* 1.3M 2.5M one-tb-to-tib))
(def dmz-client-target-size (* 0.48M 1.5M one-tb-to-tib))
(def dmz-combined-target-size (+ dmz-server-target-size dmz-client-target-size))

(defrecord Drive [drive-size drive-cost])
(def one-tb-drive (Drive. 1 59.99M))
(def four-tb-drive (Drive. 4 121.11M))
(def six-tb-drive (Drive. 6 179.99M))
(def eight-tb-drive (Drive. 8 226.70M))
(def ten-tb-drive (Drive. 10 304.48M))
(def twelve-tb-drive (Drive. 12 399.89M))

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

(defn- make-possibilities [number r]
  (map #(repeat % r) (range 0 number)))

(defn- valid-configurations [percent-key drive-arrays case target-size]
  (let [block-range (inc (int (Math/floor (/ (:drives case) (:number-drives (first drive-arrays))))))
        all-drive-array-configurations (map flatten (apply combo/cartesian-product
                              (map #(make-possibilities block-range %) drive-arrays)))
        configurations-with-right-number-of-drive-arrays (filter #(> block-range (count %))
                                                                 all-drive-array-configurations)
        configurations-with-right-target-size (filter #(< target-size (reduce (fn [r v] (+ r (percent-key v))) 0 %))
                                                      configurations-with-right-number-of-drive-arrays)]
    (map (fn [v] {:valid-drive-array-configuration v :case case}) configurations-with-right-target-size)))

(defn- generate-all-valid-configurations [[das cs sizes]]
  (let [the-valid-configurations (map (fn [da c size]
                                        (valid-configurations :tib-50-percent da c size))
                                      das cs sizes)]
    (apply combo/cartesian-product the-valid-configurations)))

(defn- drive-size [drive-array]
  (:drive-size (:drive drive-array)))

(defn- drive-cost [drive-array]
  (:drive-cost (:drive drive-array)))

(defn- cost-of-drive-array [drive-array]
  (* (drive-cost drive-array) (:number-drives drive-array)))

(defn- final-cost-of-configuration [{:keys [valid-drive-array-configuration case]}]
  (let [number-drives (:number-drives (first valid-drive-array-configuration))]
    {:the-cost            (reduce (fn [r v] (+ r (cost-of-drive-array v))) 0 valid-drive-array-configuration)
     :the-additional-cost (:additional-cost case)
     :count-4tb           (* number-drives
                             (count (filter #(= (drive-size %)
                                                (:drive-size four-tb-drive))
                                            valid-drive-array-configuration)))
     :count-1tb           (* number-drives
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
                                 :details s})})

(defn- retrieve-the-total-cost [c]
  (if c
    (:the-total-cost (first (:valid-storage-systems c)))
    0.00M))

(defn- retrieve-the-total-drives [c]
  (println "retrieve!")
  (if c
    (let [details (:details (first (:valid-storage-systems c)))]
      (apply + (map #(:number-drives (first (:valid-drive-array-configuration %))) details)))
    0))

(defn- generate-cheapest-valid-storage-systems [l]
  (let [result (reduce (fn [r v] (let [vss (generate-valid-storage-systems v)
                                       the-total-cost-vss (retrieve-the-total-cost vss)
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
                       nil (generate-all-valid-configurations l))]
    (println (retrieve-the-total-cost result))
    result))

(defn- configuration-to-drive-array-names [c]
  (map #(str (:array-name %) " x" (:number-drives %)) c))

(defn- output-configuration [{:keys [valid-drive-array-configuration case]}]
  (println "----")
  (println (:name case))
  (println (configuration-to-drive-array-names valid-drive-array-configuration)))

(defn -main []
  (let [combined (combo/cartesian-product (list (list raid-one-z-three-drive-arrays)
                                                (list raid-one-z-four-drive-arrays)
                                                (list raid-one-z-five-drive-arrays)
                                                (list mirror-drive-arrays))
                                          (list (list one-r5 one-phanteks-itx)
                                                (list one-xl one-phanteks-itx))
                                          (list (list lan-combined-target-size dmz-combined-target-size)))
        separate (combo/cartesian-product (combo/selections (list raid-one-z-three-drive-arrays
                                                                  raid-one-z-four-drive-arrays
                                                                  raid-one-z-five-drive-arrays
                                                                  mirror-drive-arrays) 2)
                                          (list (list one-r5 one-r5 one-phanteks-itx)
                                                (list one-xl one-xl one-phanteks-itx))
                                          (list (list lan-server-target-size
                                                      lan-client-target-size
                                                      dmz-combined-target-size)))
        all-valid-storage-systems (apply merge-with
                                         concat
                                         (filter identity
                                                 (map generate-cheapest-valid-storage-systems
                                                      (concat combined separate))))
        sorted-configurations (sort-by :the-total-cost
                                       (:valid-storage-systems all-valid-storage-systems))]
    (println "----")
    (println (:the-total-cost (first sorted-configurations)))
    (dorun (map #(output-configuration %) (:details (first sorted-configurations))))))



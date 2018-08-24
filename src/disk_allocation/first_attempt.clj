(ns disk-allocation.first-attempt
  (:use [disk-allocation.data])
  (:require [disk-allocation.utilities :as utils])
  (:require [clojure.math.combinatorics :as combo]))

(defn- drive-size [drive-array]
  (:drive-size (:drive drive-array)))

(defn- drive-cost [drive-array]
  (:drive-cost (:drive drive-array)))

(defn- cost-of-drive-array [drive-array]
  (* (drive-cost drive-array) (:number-drives drive-array)))

(defn- final-cost-of-configuration [{:keys [valid-drive-array-configuration case]}]
  (let [number-drives-in-array (:number-drives (first valid-drive-array-configuration))]
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

(defn- make-possibilities [number r]
  (map #(repeat % r) (range 0 number)))

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
        configurations-with-right-physical-drive-sizes (filter (partial utils/is-right-physical-drive-size case)
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

(defn- generate-cheapest-valid-storage-systems [l]
  (let [result (reduce (fn [r v] (let [vss (generate-valid-storage-systems v)]
                                   (replace-if-cheaper vss r)))
                       nil (generate-all-valid-machines-in-system l))]
    (println (retrieve-the-total-cost result))
    result))

(defn- configuration-to-drive-array-names [c]
  (map #(str (:array-name %) " x" (:number-drives %)) c))

(defn- output-configuration [{:keys [valid-drive-array-configuration case]}]
  (println "----")
  (println (:name case))
  (println (configuration-to-drive-array-names valid-drive-array-configuration)))

(defn find-cheapest-system
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

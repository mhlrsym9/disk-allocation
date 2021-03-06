(ns disk-allocation.utilities
  (:use [disk-allocation.data]))

(defn is-right-physical-drive-size [case drive-arrays-configuration]
  (if (seq drive-arrays-configuration)
    (if (:can-be-two-point-five-drive (first drive-arrays-configuration))
      true
      (<= (* (:number-drives (first drive-arrays-configuration))
             (count drive-arrays-configuration))
          (:three-point-five-drives case)))
    true))

(defn- calculate-dac-size [dac percent-key]
  (reduce (fn [r da] (+ r (percent-key da))) 0.0 dac))

(defn is-dac-right-size? [target-size dac percent-key]
  (let [dac-size (calculate-dac-size dac percent-key)]
    (cond
      (= target-size lan-client-target-size) (and (<= lan-client-target-size dac-size)
                                                  (<= dac-size (* 1.2 lan-client-target-size)))
      (= target-size lan-server-target-size) (and (<= lan-server-target-size dac-size)
                                                  (<= dac-size (* 1.2 lan-server-target-size)))
      (= target-size lan-combined-target-size) (and (<= lan-combined-target-size dac-size)
                                                    (<= dac-size (* 1.1 lan-combined-target-size)))
      (= target-size dmz-client-target-size) (and (<= dmz-client-target-size dac-size)
                                                  (<= dac-size (* 6.0 dmz-client-target-size)))
      (= target-size dmz-server-target-size) (and (<= dmz-server-target-size dac-size)
                                                  (<= dac-size (* 2.0 dmz-server-target-size)))
      (= target-size dmz-combined-target-size) (and (<= dmz-combined-target-size dac-size)
                                                    (<= dac-size (* 2.0 dmz-combined-target-size)))
      :else (<= target-size dac-size))))

(defn- number-of-two-point-five-drives-in-dac [dac]
  (reduce (fn [r {:keys [number-drives], {:keys [can-be-two-point-five-drive]} :drive}]
            (+ r (if can-be-two-point-five-drive
                   number-drives
                   0))) 0 dac))

(defn total-number-drives-in-dac [dac]
  (reduce (fn [r {:keys [number-drives]}] (+ r number-drives)) 0 dac))

(defn does-dac-have-right-physical-drive-size-configuration? [{:keys [max-number-drives number-two-point-five-drives]} dac]
  (cond
    (<= (total-number-drives-in-dac dac) (- max-number-drives number-two-point-five-drives)) true
    (< 0 number-two-point-five-drives) (<= number-two-point-five-drives
                                           (number-of-two-point-five-drives-in-dac dac))
    :else true))

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
       :number-two-point-five-drives (max 0 (- two-point-five-drives
                                               (- max-number-drives-in-case
                                                  max-number-drives-in-mb)))})))

(defn- make-drive-adjustment [{:keys [max-number-drives number-two-point-five-drives]}
                              {{:keys [three-point-five-drives-required
                                       two-point-five-drives-required]} :rd}]
  (let [^int two-point-five-drives-remaining (- number-two-point-five-drives
                                                two-point-five-drives-required)
        final-two-point-five-drives (max 0 two-point-five-drives-remaining)
        final-three-point-five-drives (- max-number-drives
                                         number-two-point-five-drives
                                         three-point-five-drives-required
                                         (if (< two-point-five-drives-remaining 0)
                                           (Math/abs two-point-five-drives-remaining)
                                           0))]
    {:max-number-drives (+ final-three-point-five-drives final-two-point-five-drives)
     :number-two-point-five-drives final-two-point-five-drives}))

(defn- target-size-for-machine [{:keys [target-size]}]
  {:target-size (first target-size)})

(defn generate-machine-configuration-pattern-v2 [mc]
  (merge (max-number-drives-for-machine mc) (target-size-for-machine mc)))

(defn generate-storage-machine-configuration-pattern-v2 [smc]
  (map generate-machine-configuration-pattern-v2 smc))

(defn- target-size-for-machine-v3 [{:keys [target-size]}]
  {:target-size target-size})

(defn- all-drive-arrays-for-machine-v3 [{:keys [all-drive-arrays]}]
  {:all-drive-arrays all-drive-arrays})

(defn generate-storage-machine-configuration-pattern-v3 [mc]
  (merge (make-drive-adjustment (max-number-drives-for-machine mc) mc)
         (target-size-for-machine-v3 mc)
         (all-drive-arrays-for-machine-v3 mc)))

(defn generate-storage-farm-configuration-pattern-v3 [smc]
  (map generate-storage-machine-configuration-pattern-v3 smc))

(defn number-drives-in-dac [{:keys [drive-size]} dac]
  (reduce (fn [r da] (+ r (if (= drive-size (:drive-size (:drive da)))
                            (:number-drives da)
                            0))) 0 dac))

(defn- number-drives-in-storage-configuration [drive sc]
  (reduce (fn [r dac] (+ r (number-drives-in-dac drive dac))) 0 sc))

(defn calculate-drive-adjustment [sc scp]
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
    (+ (* (:drive-cost four-tb-drive) (min (number-drives-in-storage-configuration four-tb-drive sc) 9))
       (* (:drive-cost one-tb-drive) (min number-one-tb-three-point-five-drives-needed (+ 2 number-additional-one-tb-two-point-five-drives)))
       (* (:drive-cost one-tb-drive) (min number-one-tb-two-point-five-drives-needed 2)))))






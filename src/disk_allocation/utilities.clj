(ns disk-allocation.utilities)

(defn is-right-physical-drive-size [case drive-arrays-configuration]
  (if (seq drive-arrays-configuration)
    (if (:can-be-two-point-five-drive (first drive-arrays-configuration))
      true
      (<= (* (:number-drives (first drive-arrays-configuration))
             (count drive-arrays-configuration))
          (:three-point-five-drives case)))
    true))


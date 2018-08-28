(ns disk-allocation.data
  (:require [clojure.math.combinatorics :as combo]))

(defrecord Cpu [cost name])
(def e5-2603-v3 (->Cpu 221.00M "e5-2603-v3"))
(def e5-2603-v4 (->Cpu 229.99M "e5-2603-v4"))
(def g3900 (->Cpu 36.99M "g3900"))
(def g3930 (->Cpu 35.99M "g3930"))
(def atom-c2750 (->Cpu 0.01M "atom-c2750"))
(def cheap-cpu (->Cpu 0.00M "cheap cpu"))

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

(defrecord Machine [^Case case ^Motherboard mb ^Cpu cpu ^Hba hba target-size type])

(defn- generate-machines [all-components]
  (map (fn [[case mb cpu hba size type]] (->Machine case mb cpu hba size type)) all-components))

(def list-of-lan-cases (list one-r5 one-xl one-define-mini one-silencio))
(comment (def list-of-lan-cases (list one-r5)))
(def all-lan-servers (generate-machines (combo/cartesian-product list-of-lan-cases
                                                                 (list asrock-x99m)
                                                                 (list e5-2603-v4)
                                                                 (list hba-none)
                                                                 (list nil)
                                                                 (list :lan))))

(def list-of-dmz-cases (list one-r5 one-xl one-define-mini one-silencio one-phanteks-itx))
(comment (def list-of-dmz-cases (list one-phanteks-itx)))
(def all-dmz-servers (generate-machines (combo/cartesian-product list-of-dmz-cases
                                                                 (list ga-9sisl)
                                                                 (list atom-c2750)
                                                                 (list hba-none)
                                                                 (list nil)
                                                                 (list :dmz))))

(def all-storage-in-one-machine
  (map list
       (generate-machines (combo/cartesian-product (list one-r5)
                                                   (list augmented-msi-x99a-tomahawk)
                                                   (list e5-2603-v3)
                                                   (list nil hba-9211-4i hba-9211-8i)
                                                   (list (list lan-combined-target-size dmz-combined-target-size))
                                                   (list :lan-and-dmz)))))

(defn- big-storage-box [size-list machine-type]
  (generate-machines (combo/cartesian-product (list one-r5 one-xl)
                                              (list msi-x99a-tomahawk)
                                              (list e5-2603-v3)
                                              (list hba-none hba-9211-4i hba-9211-8i)
                                              size-list
                                              (list machine-type))))

(defn- small-storage-box [size-list machine-type]
  (generate-machines (combo/cartesian-product (list one-r5 one-xl one-silencio one-define-mini)
                                              (list supermicro-x11ssh)
                                              (list g3900 g3930)
                                              (list hba-none hba-9211-4i hba-9211-8i)
                                              size-list
                                              (list machine-type))))

(def all-storage-in-two-machines
  (combo/cartesian-product
    (big-storage-box (list (list lan-combined-target-size)) :lan)
    (small-storage-box (list (list dmz-combined-target-size)) :dmz)))

(def storage-with-lan-split
  (combo/cartesian-product
    (big-storage-box (list (list lan-server-target-size)) :lan)
    (small-storage-box (list (list lan-client-target-size)) :lan)
    (small-storage-box (list (list dmz-combined-target-size)) :dmz)))

(def storage-with-dmz-split
  (combo/cartesian-product
    (big-storage-box (list (list lan-combined-target-size)) :lan)
    (small-storage-box (list (list dmz-server-target-size)) :dmz)
    (small-storage-box (list (list dmz-client-target-size)) :dmz)))

(def all-storage-in-four-machines
  (combo/cartesian-product
    (big-storage-box (list (list lan-server-target-size)) :lan)
    (small-storage-box (list (list lan-client-target-size)) :lan)
    (small-storage-box (list (list dmz-server-target-size)) :dmz)
    (small-storage-box (list (list dmz-client-target-size)) :dmz)))

(def list-of-all-storage-machine-configuration-lists (list all-storage-in-two-machines
                                                           storage-with-lan-split
                                                           storage-with-dmz-split
                                                           all-storage-in-four-machines))

(defrecord Drive [drive-size can-be-two-point-five-drive drive-cost])
(def one-tb-drive (Drive. 1 true 59.99M))
(def four-tb-drive (Drive. 4 false (/ 193.99M 2)))          ; NewEgg 2018-08-24 flyer, ends 2018-08-27
(def six-tb-drive (Drive. 6 false 154.99M))                 ; NewEgg 2018-08-24 flyer, ends 2018-08-27
(def eight-tb-drive (Drive. 8 false 226.70M))
(def ten-tb-drive (Drive. 10 false 289.99M))                ; NewEgg 2018-08-21 flyer, ends 2018-08-24
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


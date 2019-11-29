(ns disk-allocation.data
  (:require [clojure.math.combinatorics :as combo]))

(defrecord Cpu [cost name])
(def e5-2603-v3 (->Cpu 221.00M "e5-2603-v3"))
(def e5-2603-v4 (->Cpu 229.99M "e5-2603-v4"))
(def g3900 (->Cpu 36.99M "g3900"))
(def g3930 (->Cpu 35.99M "g3930"))
(def atom-c2750 (->Cpu 0.01M "atom-c2750"))
(def cheap-cpu (->Cpu 0.00M "cheap cpu"))
(def placeholder-cpu (->Cpu 0.00M "placeholder CPU"))

(defrecord Hba [additional-sata-connectors cost name])
(def hba-9211-4i (->Hba 4 75.50M "9211-4i"))
(def hba-9211-8i (->Hba 8 78.00M "9211-8i"))
(def hba-none (->Hba 0 0.00M "no hba"))

(defrecord Motherboard [number-sata-connections cost name])
(def msi-x99a-tomahawk (->Motherboard 10 (+ 104.99M 235.99M) "MSI X99A Tomahawk")) ; 16GB ECC memory
(def augmented-msi-x99a-tomahawk (->Motherboard 10 (+ 104.99M 235.99M 235.99M) "MSI X99A Tomahawk w/ extra memory")) ; 32GB ECC memory
(def asrock-x99m (->Motherboard 10 (+ 199.99M 53.11M 53.11M) "ASRock X99M")) ; DeepServer3
(def ga-9sisl (->Motherboard 6 (- (+ 233.99M 98.00M) (:cost atom-c2750)) "GA-9SISL")) ; DeepDivide
(def supermicro-x11ssh (->Motherboard 8 (+ 198.99M 117.99M) "SuperMicro X11SSH")) ; Motherboard plus 8GB ECC memory
(def augmented-supermicro-x11ssh (->Motherboard 8 (+ 198.99M 235.99M) "SuperMicro X11SSH w/ extra memory")) ; Motherboard plus 16GB ECC memory
(def placeholder-mb (->Motherboard 0 0.00M "placeholder MB"))

(defrecord Case [three-point-five-drives two-point-five-drives cost name])
(def one-r5 (->Case 11 2 123.80M "r5"))
(def one-xl (->Case 14 0 (+ 132.45M 11.99M) "xl"))
(def one-phanteks-itx (->Case 3 1 89.99M "Phanteks ITX"))
(def one-define-mini (->Case 9 0 (* 2 (+ 123.52M 39.97M)) "Mini"))
(def one-silencio (->Case 4 4 79.99M "Silencio"))
(def one-norco (->Case 16 0 331.41M "Norco"))
(def placeholder-case (->Case 0 0 0.00M "placeholder case"))

(defrecord RequiredDrives [three-point-five-drives-required two-point-five-drives-required name])
(def no-required-drives (->RequiredDrives 0 0 "no required"))
(def lan-required-drives (->RequiredDrives 3 0 "lan required"))
(def dmz-required-drives (->RequiredDrives 1 1 "dmz required"))

(def one-tb-to-tib 0.909495M)
(def lan-server-target-size (* 9.128M 2.5M one-tb-to-tib))
(def lan-client-target-size (* 5.812M 1.5M one-tb-to-tib))
(def lan-combined-target-size (+ lan-server-target-size lan-client-target-size))

(def dmz-server-target-size (* 1.3M 2.5M one-tb-to-tib))
(def dmz-client-target-size (* 0.48M 1.5M one-tb-to-tib))
(def dmz-combined-target-size (+ dmz-server-target-size dmz-client-target-size))

(defrecord Machine [^Case case ^Motherboard mb ^Cpu cpu ^Hba hba ^RequiredDrives rd target-size type all-drive-arrays])

(defn- generate-machines [all-components]
  (map (fn [[case mb cpu hba rd size type all-drive-arrays]]
         (->Machine case mb cpu hba rd size type all-drive-arrays))
       all-components))

(def list-of-lan-cases (list one-r5 one-xl one-define-mini one-silencio))
(def all-possible-lan-machines (generate-machines (combo/cartesian-product list-of-lan-cases
                                                                           (list asrock-x99m)
                                                                           (list e5-2603-v4)
                                                                           (list hba-none)
                                                                           (list no-required-drives)
                                                                           (list nil)
                                                                           (list :lan)
                                                                           (list nil))))

(def list-of-dmz-cases (list one-r5 one-xl one-define-mini one-silencio one-phanteks-itx))
(def all-possible-dmz-machines (generate-machines (combo/cartesian-product list-of-dmz-cases
                                                                           (list ga-9sisl)
                                                                           (list atom-c2750)
                                                                           (list hba-none)
                                                                           (list no-required-drives)
                                                                           (list nil)
                                                                           (list :dmz)
                                                                           (list nil))))

(defn generate-standard-pool [name storage-farm-configuration-pool]
  {:configuration                   :farm-configuration-pool
   :name                            name
   :storage-farm-configuration-pool storage-farm-configuration-pool
   :lan-pool                        all-possible-lan-machines
   :dmz-pool                        all-possible-dmz-machines})

(def placeholder-machines (generate-machines (combo/cartesian-product (list placeholder-case)
                                                                      (list placeholder-mb)
                                                                      (list placeholder-cpu)
                                                                      (list hba-none)
                                                                      (list no-required-drives)
                                                                      (list nil)
                                                                      (list :placeholder)
                                                                      (list nil))))

(defn generate-placeholder-pool [name storage-farm-configuration-pool]
  {:configuration                   :farm-configuration-pool
   :name                            name
   :storage-farm-configuration-pool storage-farm-configuration-pool
   :lan-pool                        placeholder-machines
   :dmz-pool                        placeholder-machines})

(defrecord Drive [drive-size can-be-two-point-five-drive drive-cost])

(def one-tb-drive (Drive. 1 true (with-precision 5 (/ (+ 53.99M 59.99M) 2M))))              ; Ave NewEgg price 2019-11-29

;(def two-tb-drive (Drive. 2 false (with-precision 5 (/ (+ 77.05M 81.95M) 2M))))             ; Ave NewEgg price 2018-12-24
(def two-tb-drive (Drive. 2 false 59.99M))                                                 ; NewEgg sale ends 2019-11-29

(def three-tb-drive (Drive. 3 false (with-precision 5 (/ (+ 75.99M 89.99M) 2M))))          ; Ave NewEgg price 2019-11-29
;(def three-tb-drive (Drive. 3 false 75.00M))                                               ; NewEgg sale ends 2019-01-29

;(def four-tb-drive (Drive. 4 false (with-precision 5 (/ (+ 119.97M 119.99M 124.00M 129.99M) 4M)))) ; Ave NewEgg price 2018-12-24
(def four-tb-drive (Drive. 4 false 89.99M))                                                      ; NewEgg sale ends 2019-12-01

;(def six-tb-drive (Drive. 6 false (with-precision 5 (/ (+ 168.99M 344.99M 184.46M 184.98M) 5M))))  ; Ave NewEgg price 2018-09-03
(def six-tb-drive (Drive. 6 false 129.99M))                                                         ; NewEgg sale ends 2019-11-29

;(def eight-tb-drive (Drive. 8 false (with-precision 5 (/ (+ 189.99M 200.00M) 2M)))) ; Ave NewEgg price 2019-11-29
(def eight-tb-drive (Drive. 8 false 149.99M))                                                 ; NewEgg sale ends 2019-11-28

(def ten-tb-drive (Drive. 10 false (with-precision 5 (/ (+ 262.99M 279.99M) 2M)))) ; Ave NewEgg price 2019-11-29
;(def ten-tb-drive (Drive. 10 false 249.99M))                                                ; NewEgg sale ends 2019-03-29

(def twelve-tb-drive (Drive. 12 false 279.99M))                                             ; NewEgg price 2019-11-29
;(def twelve-tb-drive (Drive. 12 false 329.99M))                                            ; NewEgg sale ends 2019-03-23

(def fourteen-tb-drive (Drive. 14 false 469.99M))                                           ; NewEgg price 2019-11-29
;(def fourteen-tb-drive (Drive. 14 false 469.99M))           ; NewEgg sale ends 2019-04-02

(def sixteen-tb-drive (Drive. 16 false 540.44M))                                           ; NewEgg price 2019-11-29

(defrecord DriveArray [array-name number-drives tib-50-percent tib-80-percent drive])

(def one-tb-raid-one-z-three-drive-array (DriveArray. "1TB RAID-1Z x3" 3 0.85 1.39 one-tb-drive))
(def two-tb-raid-one-z-three-drive-array (DriveArray. "2TB RAID-1Z x3" 3 1.70 2.79 two-tb-drive))
(def three-tb-raid-one-z-three-drive-array (DriveArray. "3TB RAID-1Z x3" 3 2.54 4.16 three-tb-drive))
(def four-tb-raid-one-z-three-drive-array (DriveArray. "4TB RAID-1Z x3" 3 3.40 5.57 four-tb-drive))
(def six-tb-raid-one-z-three-drive-array (DriveArray. "6TB RAID-1Z x3" 3 5.08 8.33 six-tb-drive))
(def eight-tb-raid-one-z-three-drive-array (DriveArray. "8TB RAID-1Z x3" 3 6.79 11.15 eight-tb-drive))
(def ten-tb-raid-one-z-three-drive-array (DriveArray. "10TB RAID-1Z x3" 3 8.51 13.97 ten-tb-drive))
(def twelve-tb-raid-one-z-three-drive-array (DriveArray. "12TB RAID-1Z x3" 3 10.16 16.66 twelve-tb-drive))
(def fourteen-tb-raid-one-z-three-drive-array (DriveArray. "14TB RAID-1Z x3" 3 11.88 19.48 fourteen-tb-drive))
(def sixteen-tb-raid-one-z-three-drive-array (DriveArray. "16TB RAID-1Z x3" 3 14.05 22.48 sixteen-tb-drive))
(def raid-one-z-three-drive-arrays (list one-tb-raid-one-z-three-drive-array
                                         two-tb-raid-one-z-three-drive-array
                                         three-tb-raid-one-z-three-drive-array
                                         four-tb-raid-one-z-three-drive-array
                                         six-tb-raid-one-z-three-drive-array
                                         eight-tb-raid-one-z-three-drive-array
                                         ten-tb-raid-one-z-three-drive-array
                                         twelve-tb-raid-one-z-three-drive-array
                                         fourteen-tb-raid-one-z-three-drive-array
                                         sixteen-tb-raid-one-z-three-drive-array))

(def one-tb-raid-one-z-four-drive-array (DriveArray. "1TB RAID-1Z x4" 4 1.24 2.03 one-tb-drive))
(def two-tb-raid-one-z-four-drive-array (DriveArray. "2TB RAID-1Z x4" 4 2.47 4.05 two-tb-drive))
(def three-tb-raid-one-z-four-drive-array (DriveArray. "3TB RAID-1Z x4" 4 3.71 6.08 three-tb-drive))
(def four-tb-raid-one-z-four-drive-array (DriveArray. "4TB RAID-1Z x4" 4 4.94 8.11 four-tb-drive))
(def six-tb-raid-one-z-four-drive-array (DriveArray. "6TB RAID-1Z x4" 4 7.41 12.16 six-tb-drive))
(def eight-tb-raid-one-z-four-drive-array (DriveArray. "8TB RAID-1Z x4" 4 9.89 16.21 eight-tb-drive))
(def ten-tb-raid-one-z-four-drive-array (DriveArray. "10TB RAID-1Z x4" 4 12.36 20.27 ten-tb-drive))
(def twelve-tb-raid-one-z-four-drive-array (DriveArray. "12TB RAID-1Z x4" 4 14.83 24.32 twelve-tb-drive))
(def fourteen-tb-raid-one-z-four-drive-array (DriveArray. "14TB RAID-1Z x4" 4 17.22 28.23 fourteen-tb-drive))
(def sixteen-tb-raid-one-z-four-drive-array (DriveArray. "16TB RAID-1Z x4" 4 20.43 32.69 sixteen-tb-drive))
(def raid-one-z-four-drive-arrays (list one-tb-raid-one-z-four-drive-array
                                        two-tb-raid-one-z-four-drive-array
                                        three-tb-raid-one-z-four-drive-array
                                        four-tb-raid-one-z-four-drive-array
                                        six-tb-raid-one-z-four-drive-array
                                        eight-tb-raid-one-z-four-drive-array
                                        ten-tb-raid-one-z-four-drive-array
                                        twelve-tb-raid-one-z-four-drive-array
                                        fourteen-tb-raid-one-z-four-drive-array
                                        sixteen-tb-raid-one-z-four-drive-array))

(def one-tb-raid-one-z-five-drive-array (DriveArray. "1TB RAID-1Z x5" 5 1.70 2.79 one-tb-drive))
(def two-tb-raid-one-z-five-drive-array (DriveArray. "2TB RAID-1Z x5" 5 3.40 5.57 two-tb-drive))
(def three-tb-raid-one-z-five-drive-array (DriveArray. "3TB RAID-1Z x5" 5 5.11 8.40 three-tb-drive))
(def four-tb-raid-one-z-five-drive-array (DriveArray. "4TB RAID-1Z x5" 5 6.80 11.15 four-tb-drive))
(def six-tb-raid-one-z-five-drive-array (DriveArray. "6TB RAID-1Z x5" 5 10.22 16.76 six-tb-drive))
(def eight-tb-raid-one-z-five-drive-array (DriveArray. "8TB RAID-1Z x5" 5 13.59 22.29 eight-tb-drive))
(def ten-tb-raid-one-z-five-drive-array (DriveArray. "10TB RAID-1Z x5" 5 16.97 27.83 ten-tb-drive))
(def twelve-tb-raid-one-z-five-drive-array (DriveArray. "12TB RAID-1Z x5" 5 20.44 33.52 twelve-tb-drive))
(def fourteen-tb-raid-one-z-five-drive-array (DriveArray. "14TB RAID-1Z x5" 5 23.81 39.05 fourteen-tb-drive))
(def sixteen-tb-raid-one-z-five-drive-array (DriveArray. "16TB RAID-1Z x5" 5 28.09 44.95 sixteen-tb-drive))
(def raid-one-z-five-drive-arrays (list one-tb-raid-one-z-five-drive-array
                                        two-tb-raid-one-z-five-drive-array
                                        three-tb-raid-one-z-five-drive-array
                                        four-tb-raid-one-z-five-drive-array
                                        six-tb-raid-one-z-five-drive-array
                                        eight-tb-raid-one-z-five-drive-array
                                        ten-tb-raid-one-z-five-drive-array
                                        twelve-tb-raid-one-z-five-drive-array
                                        fourteen-tb-raid-one-z-five-drive-array
                                        sixteen-tb-raid-one-z-five-drive-array))

(def one-tb-raid-two-z-five-drive-array (DriveArray. "1TB RAID-2Z x5" 5 1.26 2.06 one-tb-drive))
(def two-tb-raid-two-z-five-drive-array (DriveArray. "2TB RAID-2Z x5" 5 2.52 4.13 two-tb-drive))
(def three-tb-raid-two-z-five-drive-array (DriveArray. "3TB RAID-2Z x5" 5 3.78 6.21 three-tb-drive))
(def four-tb-raid-two-z-five-drive-array (DriveArray. "4TB RAID-2Z x5" 5 5.03 8.26 four-tb-drive))
(def six-tb-raid-two-z-five-drive-array (DriveArray. "6TB RAID-2Z x5" 5 7.57 12.41 six-tb-drive))
(def eight-tb-raid-two-z-five-drive-array (DriveArray. "8TB RAID-2Z x5" 5 10.07 16.51 eight-tb-drive))
(def ten-tb-raid-two-z-five-drive-array (DriveArray. "10TB RAID-2Z x5" 5 12.57 20.61 ten-tb-drive))
(def twelve-tb-raid-two-z-five-drive-array (DriveArray. "12TB RAID-2Z x5" 5 15.54 24.83 twelve-tb-drive))
(def fourteen-tb-raid-two-z-five-drive-array (DriveArray. "14TB RAID-2Z x5" 5 17.64 28.93 fourteen-tb-drive))
(def sixteen-tb-raid-two-z-five-drive-array (DriveArray. "16TB RAID-2Z x5" 5 20.81 33.30 sixteen-tb-drive))
(def raid-two-z-five-drive-arrays (list one-tb-raid-two-z-five-drive-array
                                        two-tb-raid-two-z-five-drive-array
                                        three-tb-raid-two-z-five-drive-array
                                        four-tb-raid-two-z-five-drive-array
                                        six-tb-raid-two-z-five-drive-array
                                        eight-tb-raid-two-z-five-drive-array
                                        ten-tb-raid-two-z-five-drive-array
                                        twelve-tb-raid-two-z-five-drive-array
                                        fourteen-tb-raid-two-z-five-drive-array
                                        sixteen-tb-raid-two-z-five-drive-array))

(def one-tb-raid-two-z-six-drive-array (DriveArray. "1TB RAID-2Z x6" 6 1.70 2.79 one-tb-drive))
(def two-tb-raid-two-z-six-drive-array (DriveArray. "2TB RAID-2Z x6" 6 3.40 5.57 two-tb-drive))
(def three-tb-raid-two-z-six-drive-array (DriveArray. "3TB RAID-2Z x6" 6 5.08 8.33 three-tb-drive))
(def four-tb-raid-two-z-six-drive-array (DriveArray. "4TB RAID-2Z x6" 6 6.80 11.15 four-tb-drive))
(def six-tb-raid-two-z-six-drive-array (DriveArray. "6TB RAID-2Z x6" 6 10.16 16.66 six-tb-drive))
(def eight-tb-raid-two-z-six-drive-array (DriveArray. "8TB RAID-2Z x6" 6 13.59 22.29 eight-tb-drive))
(def ten-tb-raid-two-z-six-drive-array (DriveArray. "10TB RAID-2Z x6" 6 17.03 27.93 ten-tb-drive))
(def twelve-tb-raid-two-z-six-drive-array (DriveArray. "12TB RAID-2Z x6" 6 20.31 33.31 twelve-tb-drive))
(def fourteen-tb-raid-two-z-six-drive-array (DriveArray. "14TB RAID-2Z x6" 6 23.75 38.95 fourteen-tb-drive))
(def sixteen-tb-raid-two-z-six-drive-array (DriveArray. "16TB RAID-2Z x6" 6 28.09 44.95 sixteen-tb-drive))
(def raid-two-z-six-drive-arrays (list one-tb-raid-two-z-six-drive-array
                                       two-tb-raid-two-z-six-drive-array
                                       three-tb-raid-two-z-six-drive-array
                                       four-tb-raid-two-z-six-drive-array
                                       six-tb-raid-two-z-six-drive-array
                                       eight-tb-raid-two-z-six-drive-array
                                       ten-tb-raid-two-z-six-drive-array
                                       twelve-tb-raid-two-z-six-drive-array
                                       fourteen-tb-raid-two-z-six-drive-array
                                       sixteen-tb-raid-two-z-six-drive-array))

(def one-tb-raid-two-z-seven-drive-array (DriveArray. "1TB RAID-2Z x7" 7 1.97 3.24 one-tb-drive))
(def two-tb-raid-two-z-seven-drive-array (DriveArray. "2TB RAID-2Z x7" 7 3.95 6.47 two-tb-drive))
(def three-tb-raid-two-z-seven-drive-array (DriveArray. "3TB RAID-2Z x7" 7 5.94 9.74 three-tb-drive))
(def four-tb-raid-two-z-seven-drive-array (DriveArray. "4TB RAID-2Z x7" 7 7.89 12.94 four-tb-drive))
(def six-tb-raid-two-z-seven-drive-array (DriveArray. "6TB RAID-2Z x7" 7 11.88 19.48 six-tb-drive))
(def eight-tb-raid-two-z-seven-drive-array (DriveArray. "8TB RAID-2Z x7" 7 15.78 25.88 eight-tb-drive))
(def ten-tb-raid-two-z-seven-drive-array (DriveArray. "10TB RAID-2Z x7" 7 19.84 32.54 ten-tb-drive))
(def twelve-tb-raid-two-z-seven-drive-array (DriveArray. "12TB RAID-2Z x7" 7 23.75 38.95 twelve-tb-drive))
(def fourteen-tb-raid-two-z-seven-drive-array (DriveArray. "14TB RAID-2Z x7" 7 27.81 45.61 fourteen-tb-drive))
(def sixteen-tb-raid-two-z-seven-drive-array (DriveArray. "16TB RAID-2Z x7" 7 32.61 52.18 sixteen-tb-drive))
(def raid-two-z-seven-drive-arrays (list one-tb-raid-two-z-seven-drive-array
                                         two-tb-raid-two-z-seven-drive-array
                                         three-tb-raid-two-z-seven-drive-array
                                         four-tb-raid-two-z-seven-drive-array
                                         six-tb-raid-two-z-seven-drive-array
                                         eight-tb-raid-two-z-seven-drive-array
                                         ten-tb-raid-two-z-seven-drive-array
                                         twelve-tb-raid-two-z-seven-drive-array
                                         fourteen-tb-raid-two-z-seven-drive-array
                                         sixteen-tb-raid-two-z-seven-drive-array))

(def one-tb-raid-two-z-eight-drive-array (DriveArray. "1TB RAID-2Z x8" 8 2.42 3.96 one-tb-drive))
(def two-tb-raid-two-z-eight-drive-array (DriveArray. "2TB RAID-2Z x8" 8 4.83 7.93 two-tb-drive))
(def three-tb-raid-two-z-eight-drive-array (DriveArray. "3TB RAID-2Z x8" 8 7.25 11.89 three-tb-drive))
(def four-tb-raid-two-z-eight-drive-array (DriveArray. "4TB RAID-2Z x8" 8 9.67 15.85 four-tb-drive))
(def six-tb-raid-two-z-eight-drive-array (DriveArray. "6TB RAID-2Z x8" 8 14.50 23.78 six-tb-drive))
(def eight-tb-raid-two-z-eight-drive-array (DriveArray. "8TB RAID-2Z x8" 8 19.33 31.71 eight-tb-drive))
(def ten-tb-raid-two-z-eight-drive-array (DriveArray. "10TB RAID-2Z x8" 8 24.17 39.63 ten-tb-drive))
(def twelve-tb-raid-two-z-eight-drive-array (DriveArray. "12TB RAID-2Z x8" 8 29.00 47.56 twelve-tb-drive))
(def fourteen-tb-raid-two-z-eight-drive-array (DriveArray. "14TB RAID-2Z x8" 8 33.67 55.21 fourteen-tb-drive))
(def sixteen-tb-raid-two-z-eight-drive-array (DriveArray. "16TB RAID-2Z x8" 8 39.96 63.93 sixteen-tb-drive))
(def raid-two-z-eight-drive-arrays (list one-tb-raid-two-z-eight-drive-array
                                         two-tb-raid-two-z-eight-drive-array
                                         three-tb-raid-two-z-eight-drive-array
                                         four-tb-raid-two-z-eight-drive-array
                                         six-tb-raid-two-z-eight-drive-array
                                         eight-tb-raid-two-z-eight-drive-array
                                         ten-tb-raid-two-z-eight-drive-array
                                         twelve-tb-raid-two-z-eight-drive-array
                                         fourteen-tb-raid-two-z-eight-drive-array
                                         sixteen-tb-raid-two-z-eight-drive-array))

(def one-tb-raid-two-z-nine-drive-array (DriveArray. "1TB RAID-2Z x9" 9 2.90 4.76 one-tb-drive))
(def two-tb-raid-two-z-nine-drive-array (DriveArray. "2TB RAID-2Z x9" 9 5.80 9.52 two-tb-drive))
(def three-tb-raid-two-z-nine-drive-array (DriveArray. "3TB RAID-2Z x9" 9 8.75 14.35 three-tb-drive))
(def four-tb-raid-two-z-nine-drive-array (DriveArray. "4TB RAID-2Z x9" 9 11.61 19.04 four-tb-drive))
(def six-tb-raid-two-z-nine-drive-array (DriveArray. "6TB RAID-2Z x9" 9 17.50 28.70 six-tb-drive))
(def eight-tb-raid-two-z-nine-drive-array (DriveArray. "8TB RAID-2Z x9" 9 23.21 38.07 eight-tb-drive))
(def ten-tb-raid-two-z-nine-drive-array (DriveArray. "10TB RAID-2Z x9" 9 29.11 47.74 ten-tb-drive))
(def twelve-tb-raid-two-z-nine-drive-array (DriveArray. "12TB RAID-2Z x9" 9 35.00 57.40 twelve-tb-drive))
(def fourteen-tb-raid-two-z-nine-drive-array (DriveArray. "14TB RAID-2Z x9" 9 40.71 66.77 fourteen-tb-drive))
(def sixteen-tb-raid-two-z-nine-drive-array (DriveArray. "16TB RAID-2Z x9" 9 47.98 76.76 sixteen-tb-drive))
(def raid-two-z-nine-drive-arrays (list one-tb-raid-two-z-nine-drive-array
                                        two-tb-raid-two-z-nine-drive-array
                                        three-tb-raid-two-z-nine-drive-array
                                        four-tb-raid-two-z-nine-drive-array
                                        six-tb-raid-two-z-nine-drive-array
                                        eight-tb-raid-two-z-nine-drive-array
                                        ten-tb-raid-two-z-nine-drive-array
                                        twelve-tb-raid-two-z-nine-drive-array
                                        fourteen-tb-raid-two-z-nine-drive-array
                                        sixteen-tb-raid-two-z-nine-drive-array))

(def one-tb-raid-two-z-ten-drive-array (DriveArray. "1TB RAID-2Z x10" 10 3.24 5.31 one-tb-drive))
(def two-tb-raid-two-z-ten-drive-array (DriveArray. "2TB RAID-2Z x10" 10 6.47 10.62 two-tb-drive))
(def three-tb-raid-two-z-ten-drive-array (DriveArray. "3TB RAID-2Z x10" 10 9.73 15.96 three-tb-drive))
(def four-tb-raid-two-z-ten-drive-array (DriveArray. "4TB RAID-2Z x10" 10 12.95 21.23 four-tb-drive))
(def six-tb-raid-two-z-ten-drive-array (DriveArray. "6TB RAID-2Z x10" 10 19.46 31.92 six-tb-drive))
(def eight-tb-raid-two-z-ten-drive-array (DriveArray. "8TB RAID-2Z x10" 10 25.89 42.46 eight-tb-drive))
(def ten-tb-raid-two-z-ten-drive-array (DriveArray. "10TB RAID-2Z x10" 10 32.32 53.01 ten-tb-drive))
(def twelve-tb-raid-two-z-ten-drive-array (DriveArray. "12TB RAID-2Z x10" 10 38.93 63.84 twelve-tb-drive))
(def fourteen-tb-raid-two-z-ten-drive-array (DriveArray. "14TB RAID-2Z x10" 10 45.36 74.39 fourteen-tb-drive))
(def sixteen-tb-raid-two-z-ten-drive-array (DriveArray. "16TB RAID-2Z x10" 10 53.51 85.62 sixteen-tb-drive))
(def raid-two-z-ten-drive-arrays (list one-tb-raid-two-z-ten-drive-array
                                       two-tb-raid-two-z-ten-drive-array
                                       three-tb-raid-two-z-ten-drive-array
                                       four-tb-raid-two-z-ten-drive-array
                                       six-tb-raid-two-z-ten-drive-array
                                       eight-tb-raid-two-z-ten-drive-array
                                       ten-tb-raid-two-z-ten-drive-array
                                       twelve-tb-raid-two-z-ten-drive-array
                                       fourteen-tb-raid-two-z-ten-drive-array
                                       sixteen-tb-raid-two-z-ten-drive-array))

(def one-tb-mirror-drive-array (DriveArray. "1TB MIRROR" 2 0.42 0.70 one-tb-drive))
(def two-tb-mirror-drive-array (DriveArray. "2TB MIRROR" 2 0.85 2.39 two-tb-drive))
(def three-tb-mirror-drive-array (DriveArray. "3TB MIRROR" 2 1.27 2.09 three-tb-drive))
(def four-tb-mirror-drive-array (DriveArray. "4TB MIRROR" 2 1.69 2.79 four-tb-drive))
(def six-tb-mirror-drive-array (DriveArray. "6TB MIRROR" 2 2.55 4.18 six-tb-drive))
(def eight-tb-mirror-drive-array (DriveArray. "8TB MIRROR" 2 3.40 5.57 eight-tb-drive))
(def ten-tb-mirror-drive-array (DriveArray. "10TB MIRROR" 2 4.25 6.97 ten-tb-drive))
(def twelve-tb-mirror-drive-array (DriveArray. "12TB MIRROR" 2 5.10 8.36 twelve-tb-drive))
(def fourteen-tb-mirror-drive-array (DriveArray. "14TB MIRROR" 2 5.92 9.71 fourteen-tb-drive))
(def sixteen-tb-mirror-drive-array (DriveArray. "16TB MIRROR" 2 7.02 11.24 sixteen-tb-drive))
(def mirror-drive-arrays (list one-tb-mirror-drive-array
                               two-tb-mirror-drive-array
                               three-tb-mirror-drive-array
                               four-tb-mirror-drive-array
                               six-tb-mirror-drive-array
                               eight-tb-mirror-drive-array
                               ten-tb-mirror-drive-array
                               twelve-tb-mirror-drive-array
                               fourteen-tb-mirror-drive-array
                               sixteen-tb-mirror-drive-array))

(def all-small-drive-arrays (list raid-one-z-three-drive-arrays
                                  raid-one-z-four-drive-arrays
                                  mirror-drive-arrays))

(def all-raid-two-z-drive-arrays (list raid-two-z-five-drive-arrays
                                       raid-two-z-six-drive-arrays
                                       raid-two-z-seven-drive-arrays
                                       raid-two-z-eight-drive-arrays
                                       raid-two-z-nine-drive-arrays))

(def all-drive-arrays (concat all-small-drive-arrays all-raid-two-z-drive-arrays))

(def single-storage-machine-in-farm-configuration-pool
  (generate-standard-pool
    "all-storage-in-one-machine"
    (map list
         (generate-machines
           (combo/cartesian-product (list one-r5 one-xl)
                                    (list augmented-msi-x99a-tomahawk)
                                    (list e5-2603-v3)
                                    (list hba-none hba-9211-4i hba-9211-8i)
                                    (list no-required-drives)
                                    (list (list lan-combined-target-size
                                                dmz-combined-target-size))
                                    (list :lan-and-dmz)
                                    (list all-drive-arrays))))))

(defn- big-storage-box
  ([size-list machine-type drive-arrays]
   (big-storage-box size-list machine-type drive-arrays no-required-drives))
  ([size-list machine-type drive-arrays required-drives]
   (generate-machines (combo/cartesian-product (list one-r5 one-xl)
                                               (list msi-x99a-tomahawk)
                                               (list e5-2603-v3)
                                               (list hba-none hba-9211-4i hba-9211-8i)
                                               (list required-drives)
                                               (list size-list)
                                               (list machine-type)
                                               (list drive-arrays)))))

(defn- small-storage-box
  ([size-list machine-type drive-arrays]
   (small-storage-box size-list machine-type drive-arrays no-required-drives))
  ([size-list machine-type drive-arrays required-drives]
   (generate-machines (combo/cartesian-product (list one-r5 one-xl one-silencio one-define-mini)
                                               (list supermicro-x11ssh)
                                               (list g3900 g3930)
                                               (list hba-none hba-9211-4i hba-9211-8i)
                                               (list required-drives)
                                               (list size-list)
                                               (list machine-type)
                                               (list drive-arrays)))))

(def two-storage-machines-in-farm-configuration-pool
  (generate-standard-pool
    "all-storage-in-two-machines"
    (combo/cartesian-product
      (big-storage-box (list lan-combined-target-size) :lan all-drive-arrays)
      (small-storage-box (list dmz-combined-target-size) :dmz all-small-drive-arrays))))

(def three-storage-machines-with-lan-split-in-farm-configuration-pool
  (generate-standard-pool
    "storage-with-lan-split"
    (combo/cartesian-product
      (big-storage-box (list lan-server-target-size) :lan all-drive-arrays)
      (small-storage-box (list lan-client-target-size) :lan all-small-drive-arrays)
      (small-storage-box (list dmz-combined-target-size) :dmz all-small-drive-arrays))))

(def three-storage-machines-with-dmz-split-in-farm-configuration-pool
  (generate-standard-pool
    "storage-with-dmz-split"
    (combo/cartesian-product
      (big-storage-box (list lan-combined-target-size) :lan all-drive-arrays)
      (small-storage-box (list dmz-server-target-size) :dmz all-small-drive-arrays)
      (small-storage-box (list dmz-client-target-size) :dmz all-small-drive-arrays))))

(def placeholder-big-storage-box
  (generate-machines (combo/cartesian-product (list one-r5 one-xl)
                                              (list augmented-msi-x99a-tomahawk)
                                              (list e5-2603-v3)
                                              (list hba-none hba-9211-4i hba-9211-8i)
                                              (list no-required-drives)
                                              (list (list lan-server-target-size dmz-server-target-size))
                                              (list :lan-and-dmz)
                                              (list all-drive-arrays))))

(def placeholder-lan-storage-box
  (generate-machines (combo/cartesian-product list-of-lan-cases
                                              (list asrock-x99m)
                                              (list e5-2603-v4)
                                              (list hba-none) ; LAN has no extra slots available!
                                              (list lan-required-drives)
                                              (list (list lan-client-target-size))
                                              (list :lan)
                                              (list all-small-drive-arrays))))

(def placeholder-dmz-storage-box
  (generate-machines (combo/cartesian-product list-of-dmz-cases
                                              (list ga-9sisl)
                                              (list atom-c2750)
                                              (list hba-none)
                                              (list dmz-required-drives)
                                              (list (list dmz-client-target-size))
                                              (list :dmz)
                                              (list all-small-drive-arrays))))

(def placeholder-storage-machine-in-farm-configuration-pool
  (generate-placeholder-pool
    "storage-with-placeholders"
    (combo/cartesian-product
      placeholder-big-storage-box
      placeholder-lan-storage-box
      placeholder-dmz-storage-box)))

(def four-storage-machines-in-farm-configuration-pool
  (generate-standard-pool
    "all-storage-in-four-machines"
    (combo/cartesian-product
      (big-storage-box (list lan-server-target-size) :lan all-drive-arrays)
      (small-storage-box (list lan-client-target-size) :lan all-small-drive-arrays)
      (small-storage-box (list dmz-server-target-size) :dmz all-small-drive-arrays)
      (small-storage-box (list dmz-client-target-size) :dmz all-small-drive-arrays))))

(def all-farm-configuration-pools
  {:configuration                :all-farm-configuration-pools
   :all-farm-configuration-pools (list single-storage-machine-in-farm-configuration-pool
                                       two-storage-machines-in-farm-configuration-pool
                                       three-storage-machines-with-lan-split-in-farm-configuration-pool
                                       three-storage-machines-with-dmz-split-in-farm-configuration-pool
                                       placeholder-storage-machine-in-farm-configuration-pool
                                       four-storage-machines-in-farm-configuration-pool)})

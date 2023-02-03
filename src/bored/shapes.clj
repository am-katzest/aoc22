(ns bored.shapes
  (:require [scad-clj.model :refer :all]
            [scad-clj.scad :refer :all]
            [clojure.string :as s]))

(defn cubulate [x]
  (union x
         (mirror [1 0 1] x)
         (mirror [0 1 1] x)))

(defn angles [c] (for [i (range c)]
                   (* i 2 pi (/ c))))

(def ra (/ pi 2))

(defn spread [offset c x]
  (apply union
         (for [i (angles c)]
           (translate [(* offset (Math/sin i))
                       (* offset (Math/cos i))
                       0] x))))

(def cubz
  (->> (cylinder 8 100)
       (spread 16 4)
       (rotate [0 0 666])
       cubulate
       (intersection (sphere 20))
       (difference (sphere 18))))

(spit "shapes.scad" (write-scad [[:fn 256] cubz]))

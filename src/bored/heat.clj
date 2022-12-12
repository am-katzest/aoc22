(ns bored.heat
  (:require [incanter [core :as i] [charts :as c]]))

(defn show [arr]
  (let [x (-> arr count)
        y (-> arr first count)
        arr (vec (reverse arr))]
    (i/view (c/heat-map #(get-in arr [(int %1) (int %2)]) 0 x 0 y :xres x :yres y))))
(defn show-path [arr steps]
  (let [y (-> arr count)
        x (-> arr first count)
        arr (vec (reverse arr))
        graph (c/heat-map #(get-in arr [(int %2) (int %1)]) 0 x 0 y :xres x :yres y)]
    (doseq [[i [y x]] (map-indexed vector steps)]
      (c/add-text graph (- x 0.5) (- y 0.5) "#"))
    (i/view graph)))

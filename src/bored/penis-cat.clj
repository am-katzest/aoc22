(ns bored.shapes
  (:use scad-clj.model)
  (:use scad-clj.scad))

(def rad->deg2
  (fn [radians]
    (if (number? radians)
      (rad->deg radians)
      (str "(" radians "*" (/ 180 pi) ")"))))

(defmethod write-expr :rotatec [depth [form [x y z] & block]]
  (concat
   (list (indent depth) "rotate ([" (rad->deg2 x) "," (rad->deg2 y) "," (rad->deg2 z) "]) {\n")
   (write-block depth block)
   (list (indent depth) "}\n")))

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
(defn cub [x] (cube x x x))

(def cubz
  (->> (difference
        (->>
         (cylinder 5 100)
         (spread 12 4)
         (rotate [0 0 (str "$t*" pi)])
         (cubulate))
        (->>
         (cylinder 4 1001)
         (spread 10 4)
         (rotate [0 0 (str "$t*" pi)])
         (cubulate)))
       (difference (sphere 10))))

(spit "shapes.scad" (write-scad [[:fn 128] cubz]))

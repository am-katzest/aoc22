(ns bored.penis-cat
  (:use scad-clj.model)
  (:use scad-clj.scad))
(def uwu
  (scale [0.1 0.1 0.05]
         (apply union
                (for [i (range 0 5 1)
                      j (range 0 5 1)
                      :let [filename (format "cat%dx%d.png" i j)
                            x (* 175 i)
                            y (* -179 j)]]
                  (translate [x y 0] (surface filename))))))
(spit "shapes.scad" (write-scad [[:fn 128]] uwu))

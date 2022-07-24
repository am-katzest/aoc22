(ns bored.dots)
(defonce frame (java.awt.Frame.))
(def size 255)
(def scale 3)
(def sc (partial * scale))
(def off (partial + 50))
(.setSize frame (java.awt.Dimension. (off (off (sc size))) (off (off (sc size)))))
(.setTitle frame "Dots")
(.setResizable frame false)
(.setVisible frame true)

(.setBackground frame (java.awt.Color. 53 3 99))
;; (java.awt.Color.getHSBColor 5 0 0)
(defn paint [x y c]
  (let [color (java.awt.Color. c c c)]
    (doto (.getGraphics frame)
      (.setColor color)
      (.fillRect (off (sc x)) (off (sc y))  (sc) (sc)))))
(defn rescale [x] (->> size
                       /
                       (* 255 x)
                       (min 255)
                       (max 0)
                       int))

(defn paint-fn [fun]
  (dorun (pmap (fn [[x y]] (paint x y (rescale (fun x y))))
               (for [x (range size)
                     y (range size)]
                 [x y]))))
(paint-fn (fn [x y] (* 55 (Math/cos (/ y 10)) (Math/sin (/ x 10)))))
(int (rescale 400))

(Thread/sleep 1000)
(prn 'uwu)
(.setVisible frame false)

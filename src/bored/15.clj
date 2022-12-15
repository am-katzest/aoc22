(ns bored.15
  (:require
   [clojure.string :as s]))

(defn  merge [[a A] [b B]]
  [(min a b) (max A B)])

(defn overlap? [[a A] [b B]]
  (or (<= a b A)
      (<= a B A)
      (<= b a B)))

(defn manhattan [[a A] [b B]]
  (+ (Math/abs (- a b))
     (Math/abs (- A B))))

(defn pair->point [[s b]]
  [s (manhattan s b)])
"plane is [x-offset direction]"
(defn point->planes [[[x y] size]]
  [[(+ x y size 1) 1]
   [(+ x y (- size) (- 1)) 1]
   [(+ x (- y) size 1) -1]
   [(+ x (- y) (- size) (- 1)) -1]])

(defn point->angled-interval [[offset angle] [[x y] s]]
  (let [x-pos-at-center (+ offset (* angle y))
        distance (- x x-pos-at-center)
        len (- s 2)]
    (when (> s (Math/abs distance))
      (if (odd? distance)   ;krótkie?
        (let [center (+ y (/ (dec distance) 2))]
          [(inc (- center len))
           (+ center len)])
        (let [center (+ y (/ distance 2))]
          [(- center len)
           (+ center len)])
        ;; [(- y (- s 2)) (+ y (- s 2))]
        ))))

(defn point->interval [y-att [[x y] s]]
  (let [distance (Math/abs (- y-att y))
        length (- s distance)]
    (when (not (neg? length))
      [(- x length)
       (+ x length)])))

(defn merge-intervals [coll n]
  (if-let [intersecting (first (filter #(overlap? n %) coll))]
    (recur (remove #{intersecting} coll) (merge intersecting n))
    (cons n coll)))

(defn len [[a A]]
  (- A a))

(let [data (->> "input15b"
                slurp
                s/split-lines
                (mapv (fn [l] (->> (s/split l #"[^0-9-]+")
                                   rest
                                   (map #(Integer/parseInt %))
                                   (partition 2)
                                   pair->point))))]
  (->> data
       (map #(point->interval 2000000 %))
       (filter some?)
       (reduce merge-intervals [])
       (map len)
       (apply +)))

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
  [[(+ x y size) -1]
   [(+ x y (- size)) -1]
   [(+ x (- y) size) 1]
   [(+ x (- y) (- size)) 1]])

(defn point->angled-interval [[offset angle] [[x y] s]]
  (let [y-at-cross (* angle (- x offset))
        y-diff (- y y-at-cross)
        vlen (if (even? y-diff)
               (int (/ (dec s) 2))
               (- (int (/ s 2)) 1/2))
        mid-cross (- y (/ y-diff 2))]
    (when (> s (Math/abs y-diff))
      [(- mid-cross vlen)
       (+ mid-cross vlen)])))

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

(defn cmfp [sensors plane]
  (->> sensors
       (map #(point->angled-interval plane %))
       (filter some?)
       (reduce merge-intervals [])))

(let [sensors (->> "input15a"
                   slurp
                   s/split-lines
                   (mapv (fn [l] (->> (s/split l #"[^0-9-]+")
                                      rest
                                      (map #(Integer/parseInt %))
                                      (partition 2)
                                      pair->point))))]
  {:part1 (->> sensors
               (map #(point->interval 2000000 %))
               (filter some?)
               (reduce merge-intervals [])
               (map len)
               (apply +))}
  {:part2 (->> sensors
               (map point->planes)
               (apply concat)
               (map #(cmfp sensors %)))})

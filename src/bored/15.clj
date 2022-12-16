(ns bored.15
  (:require
   [clojure.string :as s]))

(defn  merge [[a A] [b B]]
  [(min a b) (max A B)])

(defn overlap? [[a A] [b B]]
  (or (<= a b A)
      (<= a B A)
      (<= b a B)))

(defn touch? [[a A] [b B]]
  (or (<= (dec a) b (inc A))
      (<= (dec a) B (inc A))
      (<= (dec b) a (inc B))))

(defn intersect [[a A] [b B]]
  (when (overlap? [a A] [b B])
    [(max a b) (min A B)]))

(defn manhattan [[a A] [b B]]
  (+ (Math/abs (- a b))
     (Math/abs (- A B))))

(defn pair->point [[s b]]
  [s (inc (manhattan s b))])

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
        length (dec (- s distance))]
    (when (not (neg? length))
      [(- x length)
       (+ x length)])))

(defn merge-intervals [coll n]
  (if-let [intersecting (first (filter #(touch? n %) coll))]
    (recur (remove #{intersecting} coll) (merge intersecting n))
    (cons n coll)))

(defn len [[a A]]
  (- A a))

(def ^:dynamic mapsize 20)

(defn valids [[off angle]]
  (let [off (if (pos? angle) off (- mapsize off))]
    (intersect [0 mapsize]
               [(- off)
                (- mapsize off)])))

(defn find-gap [[a A] [b B]]
  (inc (if (< a b) A B)))

(defn cmfp [sensors [off angle :as plane]]
  (when-let [bounds (valids plane)]
    (let [ans (->> sensors
                   (map #(point->angled-interval plane %))
                   (filter some?)
                   (map #(intersect bounds %))
                   (filter some?)
                   (reduce merge-intervals []))]
      (when  (not= 1 (count ans))
        (let [y (apply find-gap ans)
              x (* angle (+ (* angle off) y))]
          (+ (* x 4000000) y))))))

(time
 (println
  (binding [mapsize 20]
    (let [sensors (->> "input15a"
                       slurp
                       s/split-lines
                       (mapv (fn [l] (->> (s/split l #"[^0-9-]+")
                                          rest
                                          (map #(Integer/parseInt %))
                                          (partition 2)
                                          pair->point))))]
      {:part1 (->> sensors
                   (map #(point->interval (/ mapsize 2) %))
                   (filter some?)
                   (reduce merge-intervals [])
                   (map len)
                   (apply +))
       :part2  (->> sensors
                    (map point->planes)
                    (apply concat)
                    (map #(cmfp sensors %))
                    (filter some?)
                    first)}
      (dotimes [_ 100]
        (->> sensors
             (map point->planes)
             (apply concat)
             (map #(cmfp sensors %))
             (filter some?)
             first))))))

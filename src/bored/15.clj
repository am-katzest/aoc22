(ns bored.15
  (:require
   [clojure.string :as s]))

;; interval logic
(defn merge-two [[a A] [b B]]
  [(min a b) (max A B)])

(defn touch? [[a A] [b B]]
  (or (<= (dec a) b (inc A))
      (<= (dec a) B (inc A))
      (<= (dec b) a (inc B))))

(defn merge-intervals [coll n]
  (if-let [intersecting (first (filter #(touch? n %) coll))]
    (recur (remove #{intersecting} coll) (merge-two intersecting n))
    (cons n coll)))
;; reading data
(defn manhattan [[a A] [b B]]
  (+ (Math/abs (- a b))
     (Math/abs (- A B))))

(defn pair->point [[s b]]
  [s (inc (manhattan s b))])
;; part 1 specific
(defn point->horizontal-interval [y-att [[x y] s]]
  (let [distance (Math/abs (- y-att y))
        length (dec (- s distance))]
    (when (not (neg? length))
      [(- x length)
       (+ x length)])))

(defn len [[a A]]
  (- A a))
;; part 2 specific
(defn point->angled-interval [[offset angle] [[x y] s]]
  (let [y-at-cross (* angle (- x offset))
        y-diff (- y y-at-cross)
        vlen (if (even? y-diff)         ; uhh, it's geometry stuff
               (int (/ (dec s) 2))
               (- (int (/ s 2)) 1/2))
        mid-cross (- y (/ y-diff 2))]
    (when (> s (Math/abs y-diff))
      [(- mid-cross vlen)
       (+ mid-cross vlen)])))

(def ^:dynamic mapsize 20)

(defn point->planes [[[x y] size]]
  (for [op-y [- +]
        op-s [- +]]
    [(+ x (op-y y) (op-s size)) (op-y -1)]))

(defn find-gap [[a A] [b B]]
  (inc (if (< a b) A B)))

(defn find-point-on-plane [sensors [off angle :as plane]]
  (let [ans (->> sensors
                 (map #(point->angled-interval plane %))
                 (filter some?)
                 (reduce merge-intervals []))]
    (when (= 2 (count ans))
      (let [y (apply find-gap ans)
            x (* angle (+ (* angle off) y))]
        (+ (* x 4000000) y)))))

(binding [mapsize 4000000]
  (let [sensors (->> "input15b"
                     slurp
                     s/split-lines
                     (mapv (fn [l] (->> (s/split l #"[^0-9-]+")
                                        rest
                                        (map #(Integer/parseInt %))
                                        (partition 2)
                                        pair->point))))]
    {:part1 (->> sensors
                 (map #(point->horizontal-interval (/ mapsize 2) %))
                 (filter some?)
                 (reduce merge-intervals [])
                 (map len)
                 (apply +))
     :part2  (->> sensors
                  (map point->planes)
                  (apply concat)
                  (map #(find-point-on-plane sensors %))
                  (filter some?)
                  first)}))

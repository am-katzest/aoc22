(ns bored.15
  (:require [clojure.string :as s]))

(defn  merge [[a A] [b B]]
  [(min a b) (max A B)])

(defn overlap? [[a A] [b B]]
  (or (<= a b A)
      (<= a B A)
      (<= b a B)))

(defn abs [x] (if (neg? x) (- x) x))

(defn manhattan [[a A] [b B]]
  (+ (abs (- a b))
     (abs (- A B))))

(defrecord angled [offset direction interval])

(defn pair->point [[s b]]
  [s (manhattan s b)])

(defn point->interval [att [[x y] s]]
  (let [distance (abs (- att y))
        length (- s distance)]
    (when (pos? length)
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
                (map (fn [l] (->> (s/split l #"[^0-9-]+")
                                  rest
                                  (map #(Integer/parseInt %))
                                  (partition 2)))))]
  (->> data
       (map #(point->interval 2000000 (pair->point %)))
       (filter some?)
       (reduce merge-intervals [])
       (map len)
       (apply +)))

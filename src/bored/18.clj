(ns bored.18
  (:require [clojure.string :as s]))

(defn read-cube [l]
  (mapv #(Integer/parseInt %)
        (s/split l #",")))

(defn neighbors [[x y z]]
  (for [[xo yo zo] [[0 0 1]
                    [0 0 -1]
                    [0 1 0]
                    [0 -1 0]
                    [1 0 0]
                    [-1 0 0]]]
    [(+ x xo)
     (+ y yo)
     (+ z zo)]))

(defn  within-bounds? [[x y z]]
  (and (>= 22 x -1)
       (>= 22 y -1)
       (>= 22 z -1)))

(defn count-faces [condition c]
  (->> c
       neighbors
       (filter condition)
       count))

(defn fill-steam [stones]
  (loop [to-expand [[0 0 0]]
         occupied stones]
    (if-let [current (peek to-expand)]
      (let [children (filter #(and (nil? (occupied %))
                                   (within-bounds? %))
                             (neighbors current))]
        (recur  (into (pop to-expand) children)
                (assoc occupied current :steam)))
      occupied)))

(let [input (->> "input18b"
                 slurp
                 (s/split-lines)
                 (map read-cube))
      s (into {} (map (fn [x] [x :stone])) input)
      s+s (fill-steam s)]
  {:part1 (->> input
               (map #(count-faces (complement s) %))
               (reduce +))
   :part2 (->> input
               (map #(count-faces
                      (fn [face]
                        (and (s+s face)
                             (not (s face))))  %))
               (reduce +))})

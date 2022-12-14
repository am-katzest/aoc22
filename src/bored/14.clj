(ns bored.14
  (:require [clojure.string :as s]))

(defn normalize [x]
  (cond (> x 0) 1
        (< x 0) -1
        :else 0))

(defn make-wall [[a b]]
  (if (= a b) [b]
      (let [dir (mapv normalize (mapv - b a))]
        (cons a (make-wall [(mapv + a dir) b])))))

(defn make-walls [line]
  (->> (s/split line #" -> |,")
       (map #(Integer/parseInt %))
       (partition 2)
       (partition 2 1)
       (map make-wall)
       (apply concat)))

(defn free? [m idx] (nil? (m idx)))

(defn on-level? [l [_ y]] (= l y))

(defn move [grain m lp]
  (first
   (for [d [[0 1] [-1 1] [1 1]]
         :let [n (mapv + grain d)]
         :when (free? m n)
         :when (not (on-level? lp n))]
     n)))

(defn drop-sand [floor-level m]
  (loop [grain [500 0]]
    (if-let [grain' (move grain m floor-level)]
      (recur grain')
      (assoc m grain :░░ :last-grain grain))))

(let [stones
      (->> "input14a"
           slurp
           (s/split-lines)
           (map make-walls)
           (apply concat))
      lowest-point (apply  max (map second stones))
      starting-map
      (->> stones
           (map (fn [a] [a :██]))
           (into {}))
      simulate (fn [finished?]
                 (->> starting-map
                      (iterate #(drop-sand (+ 2 lowest-point) %))
                      (take-while finished?)
                      count))
      last-grain-on-a-floor? #(if-let [g (:last-grain %)]
                                (not (on-level? (inc lowest-point) g)) true)
      last-grain-on-top? #(nil? (get % [500 0]))]
  (println {:part1 (dec (simulate last-grain-on-a-floor?))
            :part2 (simulate last-grain-on-top?)}))

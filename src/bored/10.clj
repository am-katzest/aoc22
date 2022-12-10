(ns bored.10
  (:require
   [clojure.string :as s]))

(defn parse-instruction [line]
  (if-let [[_ i] (re-matches #"\w+ ([0-9-]+)" line)]
    (Integer/parseInt i)))

(defn advance [[ctr _] op]
  (if op (let [ctr' (+ op ctr)] [ctr' [ctr ctr']])
      [ctr [ctr]]))

(defn draw-pixel [i v] (if (#{-1 0 1} (- i v)) "#" "."))

(defn interesting-value [i r] (* (+ 20 (* 40 i)) (nth r 19)))

(let [data (->> "input10c"
                slurp
                (s/split-lines)
                (map parse-instruction)
                (reductions advance [1 [1]])
                (map second)
                (reduce concat)
                (partition 40))
      part1 (->> data
                 (map-indexed interesting-value)
                 (reduce +))
      part2 (->> data
                 (map #(s/join "" (map-indexed draw-pixel %)))
                 (s/join "\n"))
      ]
  (printf "\npart1:%d, part2:\n%s" part1 part2))

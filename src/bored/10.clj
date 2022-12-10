(ns bored.10
  (:require
   [clojure.string :as s]))

(defn parse-instruction [line]
  (if-let [[_ i] (re-matches #"\w+ ([0-9-]+)" line)]
    (Integer/parseInt i)))

(defn advance [[ctr _] op]
  (if op (let [ctr' (+ op ctr)] [ctr' [ctr ctr']])
      [ctr [ctr]]))

(defn interesting? [i] (= 20 (mod i 40)))

(defn covered? [i v] (#{1 0 -1} (- v (mod i 40))))

(let [data (->> "input10c"
                slurp
                (s/split-lines)
                (map parse-instruction)
                (reductions advance [1 [1 1]])
                (map second)
                (reduce concat))
      part1 (->> data
                 (map-indexed (fn [i v] (if (interesting? i) (* v i) nil)))
                 (filter some?)
                 (reduce +))
      part2 (->> data
                  rest
                  (map-indexed (fn [i v] (if (covered? i v) "#" ".")))
                  (partition 40)
                  (mapv #(apply str %))
                  (reduce #(str %1 "\n" %2)))]
  (printf "\npart1:%d, part2:\n%s" part1 part2))

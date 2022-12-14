(ns bored.14
  (:require [clojure.string :as s]))

(defn V
  "applies operator to each element of vectors"
  [op & vecs] (apply mapv op vecs))

(defn normal [x]
  (cond (> x 0) 1
        (< x 0) -1
        :else 0))

(defn make-wall [[a b]]
  (if (= a b) [b]
      (let [dir (V normal (V - b a))]
        [a b dir]
        (cons a (make-wall [(V + a dir) b])))))

(defn free? [m idx]
  (nil? (m idx)))

(defn floor? [lp [_ y]]
  (= (+ 1 lp) y))

(defn expand [line]
  (->> (s/split line #" -> |,")
       (map #(Integer/parseInt %))
       (partition 2)
       (partition 2 1)
       (map make-wall)
       (apply concat)))

(defn fall-sand [lp m floor-stops]
  (let [starting [500 0]]
    (loop [loc starting]
      (if (floor? lp loc) nil
          (if-let [loc' (first
                         (for [d [[0 1] [-1 1] [1 1]]
                               :let [n (V + loc d)]
                               :when (free? m n)
                               :when (or (not floor-stops) (not (floor? lp n)))] n))]
            (recur loc')
            (assoc m loc :░░))))))

(defn display-map [m lp l r]
  (println)
  (doseq [y (range -1 (+ 2 lp))]
    (doseq [x (range (dec l) r)]
      (print (symbol (get m [x y] "  "))))
    (println)))

(let [stones
      (->> "input14b"
           slurp
           (s/split-lines)
           (map expand)
           (apply concat))
      lowest-point (inc (apply  max (map second stones)))
      leftmost-point (apply  min (map first stones))
      rightmost-point (inc (apply  max (map  first stones)))
      starting-map
      (->> stones
           (map (fn [a] [a :██]))
           (into {}))
      dm #(display-map  % lowest-point leftmost-point rightmost-point)]
  (println {:part1 (->> starting-map
                        (iterate #(fall-sand lowest-point % false))
                        (take-while some?)
                        count
                        dec)
            :part2 (->> starting-map
                        (iterate #(fall-sand lowest-point % true))
                        (take-while #(nil? (get % [500 0])))
                        count)}))

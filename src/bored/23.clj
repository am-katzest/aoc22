(ns bored.23
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn print-terr [coll]
  (println)
  (println)
  (let [ys (sort (map first coll))
        xs (sort (map second coll))]
    (doseq [x (range (first xs)
                     (inc (last xs)))]
      (println)
      (doseq [y (range (first ys) (inc (last ys)))]
        (print (if (coll [y x])  "{}" "  "))))))

(defn count-terr [coll]
  (let [ys (sort (map first coll))
        xs (sort (map second coll))]
    (count (for [x (range (first xs) (inc (last xs)))
                 y (range (first ys) (inc (last ys)))
                 :when (not (coll [y x]))]
             1))))

(def decision
  [[[0 -1] [[-1 -1] [0 -1] [1 -1]]]
   [[0 1] [[-1 1] [0 1] [1 1]]]
   [[-1 0] [[-1 -1] [-1 0] [-1 1]]]
   [[1 0] [[1 -1] [1 0] [1 1]]]])

(defn current-order [i]
  (->> decision
       (split-at (mod i 4))
       reverse
       (apply concat)))

(defn target [blocked possibilities [x y]]
  (let [offset (fn [[i j]] [(+ i x) (+ j y)])]
    (when (loop [[c & cs] '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])]
            (cond (nil? c) false
                  (blocked (offset c)) true
                  :else (recur cs)))
      (first (for [[target [a b c]] possibilities ;hateithateit
                   :when (not (or (blocked (offset a))
                                  (blocked (offset b))
                                  (blocked (offset c))))]
               (offset target))))))

(defn advance [elves i]
  (let [order (current-order i)
        targets (->> elves
                     (pmap (fn [elf]
                             (when-let [new-loc (target elves order elf)]
                               [new-loc elf])))
                     (filter some?))
        forbidden (->> targets
                       (map first)
                       frequencies
                       (keep (fn [[x i]] (when (> i 1) x)))
                       (into #{}))
        targets' (filter (fn [[t _]] (not (forbidden t))) targets)
        old-locations (into #{} (map second targets'))
        new-locations (into #{} (map first targets'))]
    (-> elves
        (set/difference old-locations)
        (set/union new-locations))))

(defn just-offset? [a b]
  (->> (map (fn [[x y] [X Y]] [(- x X) (- y Y)]) (sort a) (sort b))
       frequencies
       count
       (= 1)))

(time (let [starting (->> (for [[i line] (->> "input23b"
                                              slurp
                                              s/split-lines
                                              (map-indexed vector))
                                [j v] (->> line (map-indexed vector))
                                :when (= \# v)]
                            [j i])
                          (into #{}))]
        {:first (count-terr (reduce advance starting (range 10)))
         :second (->> (range)
                      (reductions advance starting)
                      (partition 2 1)
                      (take-while (fn [[a b]] (not (just-offset? a b))))
                      count
                      inc)}))

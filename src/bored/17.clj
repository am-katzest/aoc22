(ns bored.17
  (:require [clojure.string :as s]))

(defn make-block [shapes]
  (let [X (count (first shapes))
        Y (count shapes)]
    {:size [X Y]
     :parts (for [x (range X)
                  y (range Y)
                  :when (-> shapes (nth y) (nth x) (= \#))]
              [x y])}))

(def blocks
  (->> "input17shapes" slurp
       (s/split-lines)
       (partition-by #{""})
       (remove #{[""]})
       (map reverse)
       (map make-block)
       cycle))

(defn inside-wall? [[x y :as c] fallen]
  (or (fallen c)
      (>= 0 y)
      (not (< 0 x 8))))

(defn all-false? [p coll]
  (loop [[h & t] coll]
    (cond
      (nil? h) true
      (p h) false
      :else (recur t))))

(defn can-move? [block pos dir fallen]
  (->> block
       :parts
       (map #(mapv + pos dir %))
       (all-false? #(inside-wall? % fallen))))

(defn fall-block [[gusts m highest-pos] block]
  (let [starting-pos [3 (+ 4 highest-pos)]]
    (loop [pos starting-pos
           [gust & gusts] gusts]
      (let [pos' (if (can-move? block pos gust m)
                   (mapv + pos gust)
                   pos)]
        (if (can-move? block pos' [0 -1] m)
          (recur (mapv + pos' [0 -1]) gusts)
          [gusts
           (->> block
                :parts
                (mapv #(mapv + pos' %))
                (into m))
           (max (+ (second pos)
                   (second (:size block))
                   -1)
                highest-pos)])))))

(defn display [coll]
  (println)
  (let [max-y (inc (apply max (map second coll)))]
    (doseq [y (reverse (range max-y))]
      (println)
      (doseq [x (range 1 8)]
        (print (cond
                 (= y 0) "±"
                 (coll [x y]) "@"
                 :else " "))))))

(let [gusts (->> "input17b"
                 slurp
                 (keep {\> [1 0]
                        \< [-1 0]})
                 cycle)]
  (nth (reduce fall-block [gusts #{} 0]  (take 2022 blocks)) 2))
(->> "input17b"
     slurp
     (keep {\> [1 0]
            \< [-1 0]})
     count)

(ns bored.17
  (:require [clojure.string :as s]))

(defn cycle-i [n]
  (let [c (count n)]
    (map-indexed
     (fn [i x] (with-meta x {:idx (mod i c)}))
     (cycle n))))

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
       cycle-i))

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
                 cycle-i)
      data
      (->> blocks
           (take 4000)
           (reductions fall-block [gusts #{} 0]))]
  (->>
   data
   (map-indexed (fn [i [[g] _ h]]
                  {:gust (:idx (meta g))
                   :block  (mod  i 5)
                   :height h
                   :absolute i}))
   (group-by #(dissoc % :height :absolute))
   (keep (fn [[_ r]] (when (> (count r) 1) r)))
   (keep (fn [xs] (let [xs (sort-by :absolute xs)
                        iteration-length (->> xs
                                              (map :absolute)
                                              (partition 2 1)
                                              (map (fn [[a b]]  (- b a)))
                                              first
                                              bigint)
                        iteration-height (->> xs
                                              (map :height)
                                              (partition 2 1)
                                              (map (fn [[a b]]  (- b a)))
                                              first
                                              bigint)
                        target 1000000000000
                        height-at-some-point (:height (first xs))
                        iter-at-some-point (:absolute (first xs))
                        remaining-iters (- target iter-at-some-point)
                        repeats (bigint (/ remaining-iters iteration-length))
                        iter-gain-from-repeats (* repeats  iteration-length)
                        missing-steps (- remaining-iters iter-gain-from-repeats)]
                    (when (= 0 missing-steps)
                      (+ (* iteration-height repeats) height-at-some-point)))))))

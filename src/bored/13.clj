(ns bored.13
  (:require [clojure.edn :as edn]))

(defn order [a b]
  (condp = [(int? a) (int? b)]
    [true true] (compare a b)
    [true false] (recur [a] b)
    [false true] (recur a [b])
    [false false]
    (let [[a1 & as] a
          [b1 & bs] b]
      (if (and a1 b1)
        (let [fst (order a1 b1)]
          (if (zero? fst) (recur as bs) fst))
        (compare a b)))))

(defn indices
  "returns indices of elements in `coll` matching `p`"
  [p coll]
  (for [[i x] (map-indexed vector coll)
        :when (p x)]
    (inc i)))

(let [data
      (->> "input13b"
           slurp
           (#(str "[" % "]"))
           edn/read-string)
      dividers #{[[2]] [[6]]}]
  {:part1 (->> data
               (partition 2)
               (indices (fn [[a b]] (neg? (order a b))))
               (reduce +))
   :part2 (->> data
               (concat dividers)
               (sort order)
               (indices dividers)
               (reduce *))})

(ns bored.13
  (:require [clojure.edn :as edn]))

(defn ordered? [a b]
  (condp = [(int? a) (int? b)]
    [true true] (compare b a)
    [true false] (recur [a] b)
    [false true] (recur a [b])
    [false false]
    (let [[a1 & as] a
          [b1 & bs] b]
      (condp = [(nil? a1) (nil? b1)]
        [true true] 0
        [true false] 1
        [false true] -1
        [false false]
        (let [fst (ordered? a1 b1)]
          (if (zero? fst) (recur as bs) fst))))))

(defn indices [f coll]
  (for [[i x] (map-indexed vector coll)
        :when (f x)]
    (inc i)))

(def dividers #{[[2]] [[6]]})

(let [data
      (->> "input13b"
           slurp
           (#(str "[" % "]"))
           edn/read-string)]
  {:part1
   (->> data (partition 2)
        (indices #(pos? (apply ordered? %)))
        (reduce +))
   :part2
   (->> data
        (into dividers)
        (sort (comp -  ordered?))
        (indices dividers)
        (reduce *))})

(ns bored.core
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn split-sack [s]
  (map set (split-at (/ (count s) 2) s)))
(defn item->p [x]
  (let [i (int x)
        lowercase (> i 95)]
    (if lowercase
      (+ 1 (- i (int \a)))
      (+ 27 (- i (int \A))))))
(set/intersection #{1 2} #{3 5})
(item->p \z)
(defn line->p [line]
  (let []))
(->> "input3b"
     slurp
     s/split-lines
     (map #(->> %
                split-sack
                (apply set/intersection)
                first
                item->p))
     (reduce +))

(ns bored.core
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn split-sack [s]
  (split-at (/ (count s) 2) s))
(defn item->p [x]
  (let [i (int x)
        lowercase (> i 95)]
    (if lowercase
      (+ 1 (- i (int \a)))
      (+ 27 (- i (int \A))))))
(->> "input3b"
     slurp
     s/split-lines
     (map #(->> %
                split-sack
                (map set)
                (apply set/intersection)
                first
                item->p))
     (reduce +))
(->> "input3b"
     slurp
     s/split-lines
     (partition 3)
     (map #(->> %
                (map set)
                (apply set/intersection)
                first
                item->p))
     (reduce +))

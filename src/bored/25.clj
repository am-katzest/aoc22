(ns bored.25
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def ds->d {\= -2 \- -1 \0 0 \1 1 \2 2})
(def d->ds (set/map-invert ds->d))
(defn snafu->int [l]
  (->> l
       (map ds->d)
       (reduce (fn [acc x] (+ x (* acc 5))))))

(defn int->snafu [x]
  (if (= x 0) ""
      (let [last (mod x 5)
            sub (if (<= last 2) last
                    (- last 5))
            rem (- x sub)]
        (str (int->snafu (/ rem 5))
             (d->ds sub)))))

(->> "input25b"
     slurp
     s/split-lines
     (map  snafu->int)
     (reduce +)
     int->snafu)

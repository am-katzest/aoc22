(ns bored.22
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn read-row [i ss]
  (for [[j s] (map-indexed vector ss)
        :when (not= s \space)]
    [[i j] s]))
(defn read-instructions [instructions]
  (->> instructions
       (partition-by #{\R \L})
       (map (fn [xs] (if (#{\R \L} (first xs))
                       (first xs)
                       (Integer/parseInt (apply str xs)))))))
(let [[terrain-lines _ [instructions]] (->> "input22a"
                                            slurp
                                            s/split-lines
                                            (partition-by #{""}))
      terrain (->> terrain-lines
                   (map-indexed read-row)
                   (reduce concat)
                   (into {}))]
  (read-instructions instructions))

(defn turn [direction change]
  (mod (+ direction change) 4))

(def dir->pretty {0 :right
                  1 :down
                  2 :left
                  3 :up})

(def dir->delta {0 [1 0]
                 1 [0 -1]
                 2 [-1 0]
                 3 [0 1]})

(turn 3 1)

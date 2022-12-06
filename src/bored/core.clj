(ns bored.core
  (:require
   [clojure.string :as s]))

(defn index-of [p coll]
  (loop [i 0 [h & t] coll]
    (if (p h) i
        (recur (inc i) t))))

(defn all-unique? [coll]
  (= (count coll)
     (count (distinct coll))))

(defn packet-start [len msg]
  (->> msg (partition len 1) (index-of all-unique?) (+ len)))

(->> "input6b" slurp (packet-start 4))

(->> "input6b" slurp (packet-start 14))

(ns bored.core
  (:require
   [clojure.string :as s]))

(defn index-of [p coll]
  (loop [i 0 [h & t] coll]
    (if (p h) i (recur (inc i) t))))

(defn all-unique? [coll]
  (= (count coll)
     (count (distinct coll))))

(->> "input6b"
     slurp
     (partition 4 1)
     (index-of all-unique?)
     (+ 4))

(->> "input6b"
     slurp
     (partition 14 1)
     (index-of all-unique?)
     (+ 14))

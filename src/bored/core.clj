(ns bored.core
  (:require
   [clojure.string :as s]))

(defn biggest-so-far [coll]
  (->> coll
       (reductions
        (fn [[_ max] item]
          (if (or (nil? max) (> item max))
            [true item]
            [false max])) nil)
       rest
       (map first)))

(defn transpose [x] (apply mapv vector x))

(defn check-visibility [arr]
  (let [tran #(comp transpose % transpose)
        iter #(mapv biggest-so-far %)
        rev #(mapv (comp reverse biggest-so-far reverse) %)
        a (iter arr)
        b (rev arr)
        c ((tran iter) arr)
        d ((tran rev) arr)
        aa #(or %1 %2 %3 %4)]
    (mapv #(mapv aa %1 %2 %3 %4) a b c d)))

(->> "input8b"
     slurp
     (s/split-lines)
     (mapv (fn [l] (mapv #(Integer/parseInt (str %)) l)))
     check-visibility
     (reduce concat)
     (filter identity)
     count)

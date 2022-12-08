(ns bored.core
  (:require
   [clojure.string :as s]))

(defn biggest-so-far? [coll]
  (->> coll
       (reductions
        (fn [[_ max] item]
          (if (or (nil? max) (> item max))
            [true item]
            [false max])) nil)
       rest
       (map first)))

(defn transpose [x] (apply mapv vector x))

(defn check-visibility [look sum arr]
  (let [tran #(comp transpose % transpose)
        rev-in #(mapv reverse %)
        iter #(mapv look %)
        rev #(comp rev-in % rev-in)
        a (iter arr)
        b ((rev iter) arr)
        c ((tran iter) arr)
        d ((tran (rev iter)) arr)]
    (mapv #(mapv sum %1 %2 %3 %4) a b c d)))

(let [trees (->> "input8b"
                 slurp
                 (s/split-lines)
                 (mapv (fn [l] (mapv #(Integer/parseInt (str %)) l))))]
  (->> trees
       (check-visibility biggest-so-far? #(or %1 %2 %3 %4))
       (reduce concat)
       (filter identity)
       count))

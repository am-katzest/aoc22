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

(defn rests [coll]
  (->> coll
       (iterate rest)
       (take  (count coll))))

(defn count-until [f coll]
  (loop [acc 0
         [h & t] coll]
    (cond (nil? h) acc
          (f h) (inc acc)
          :else (recur (inc acc) t))))

(defn rolling-scenic [coll]
  (->> coll rests (mapv (fn [[c & r]] (count-until #(<= c %) r)))))

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
       count)
  (->> trees
       (check-visibility rolling-scenic *)
       (apply concat)
       (apply max)))

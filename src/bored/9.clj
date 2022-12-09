(ns bored.9
  (:require
   [clojure.string :as s]))

(defn V
  "applies operator to each element of vectors"
  [op & vecs] (apply mapv op vecs))

(defn spring
  "calculates movement based on difference"
  [vec]
  (let [split #(if (pos? %) [% 1] [(- %) -1])
        [[x xs] [y ys]] (V split vec)
        min-distance (if (> (+ x y) 2) 1 2)
        choose (fn [dst dir] (if (>= dst min-distance) dir 0))]
    (V choose [x y] [xs ys])))

(defn follow [head tail]
  (->> (V - head tail)
       spring
       (V + tail)))

(defn step [[head & tail] delta]
  (let [head'  (V + head delta)
        tail' (rest (reductions follow head' tail))]
    (cons head'  tail')))

(defn simulate [length instructions]
  (let [initial-state (repeat length [0 0])] ; min=2
    (reductions step initial-state instructions)))

(def directions
  {"U" [1 0]
   "D" [-1 0]
   "L" [0 -1]
   "R" [0 1]})

(->> "input9b"
     slurp
     (s/split-lines)
     (map (fn [line] (let [[x y] (s/split line #" ")]
                       (repeat (Integer/parseInt y) (directions x)))))
     (apply concat) ; just a list of directions here
     (simulate 10)
     (map last)     ; list of tail positions
     sort
     dedupe
     count)

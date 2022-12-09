(ns bored.9
  (:require
   [clojure.string :as s]))
(def directions {"U" [1 0]
                 "D" [-1 0]
                 "L" [0 -1]
                 "R" [0 1]})

(defn V [op & vecs]
  (apply mapv op vecs))

(defn weak [x]
  (cond (> x 1) 1
        (< x -1) -1
        :else 0))
(defn strong [x]
  (cond (>= x 1) 1
        (<= x -1) -1
        :else 0))
(defn abs [x]
  ((if (pos? x) + -) x))

(defn spring [[x y :as vec]]
  (if (< 2 (+ (abs x) (abs y)))
    (V strong vec)
    (V weak vec)))

(defn follow [head tail]
  (let [tail' (->> (V - head tail)
                   spring
                   (V + tail))]
    tail'))

(defn step [[head & tail] delta]
  (let [head'  (V + head delta)
        tail' (rest (reductions follow head' tail))]
    (cons head'  tail')))

(defn simulate [length instructions]
  (let [initial-state (repeat length [0 0])] ; min=2
    (reductions step initial-state instructions)))

(->> "input9b"
     slurp
     (s/split-lines)
     (map (fn [line] (let [[x y] (s/split line #" ")]
                       (repeat (Integer/parseInt y) (directions x)))))
     (apply concat)
     (simulate 10)
     (map last)
     sort
     dedupe
     count)

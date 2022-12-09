(ns bored.9
  (:require
   [clojure.string :as s]))

(defn V "applies operator to each element of vectors"
  [op & vecs] (apply mapv op vecs))

(defn spring-o "calculates movement based on difference"
  [[x y :as vec]]
  (letfn [(abs [x] (if (pos? x) x (- x)))
          (weak [x] (cond (> x 1) 1
                          (< x -1) -1
                          :else 0))
          (strong [x] (cond (>= x 1) 1
                            (<= x -1) -1
                            :else 0))]
    (if (< 2 (+ (abs x) (abs y)))
      (V strong vec)
      (V weak vec))))
(defn spring "calculates movement based on difference"
  [vec]
  (letfn [(split [x] (if (pos? x) [x 1] [(- x) -1]))
          (weak [x] (if (> x 1) 1 0))
          (strong [x] (if (>= x 1) 1 0))]
    (let [[[x xs] [y ys]] (V split vec)]
      (V *  [xs ys] (if (< 2 (+ x y))
                      (V strong [x y])
                      (V weak [x y]))))))

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

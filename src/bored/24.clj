(ns bored.24
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn V+ [[a A] [b B]] [(+ a b) (+ A B)])

(defn V- [[a A] [b B]] [(+ a b) (+ A B)])

(defn Vabs [[x y]] [(if (neg? x) (- x) x) (if (neg? y) (- y) y)])

(defn manhattan [a b]
  (->> (V- a b) (Vabs) (apply +)))

(def ^:dynamic terr)

(defn read-terr [coll]
  (into {:y (count coll) :x (count (first coll))}
        (for [[y line] (map-indexed vector coll)
              [x cell] (map-indexed vector line)
              :let [c [x y]
                    v (cond (and (= y 0) (= cell \.)) [:start c]
                            (and (= y (dec (count coll))) (= cell \.)) [:end c]
                            (= cell \.) nil
                            :else [[c cell] cell])]
              :when (some? v)]
          v)))
(defn advance [coll]
  (let [dirs {\v [0 -1] \< [-1 0] \> [1 0] \^ [0 1]}
        roll (fn [[x y]]
               [(mod x (:x coll))
                (mod y (:y coll))])
        moveable? (comp dirs second)
        m (filter moveable? coll)
        rest (into {} (remove moveable? coll))
        moved (map (fn [[[pos _] v]] [[(roll (V+ pos (dirs v))) v] v]) m)]
    (into rest moved)))

(defn grab-walls [coll]
  (->> coll
       (keep (fn [[k v]]
               (when (char? v)
                 (first k))))
       (into #{})))

(let [init (->> "input24aa"
                slurp
                s/split-lines
                read-terr)
      terr (->>
            init
            (iterate advance)
            (map grab-walls)
            (take 50)
            (into []))]
  terr)

(ns bored.21
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as s]
            [clojure.core.logic.fd :as fd]
            [clojure.core.logic :refer :all]))

(defn read-monkey [str]
  (let [i #(Integer/parseInt %)
        [monkey a b c] (s/split str #"[ :]+")]
    [monkey (if b
              {:eq [b a c]}
              {:value (i a)})]))

(defn  replace-vals [coll f]
  (into {} (map (fn [[k _]] [k (f)]) coll)))

(defn monkeys [monkeys]
  (let [mvars (replace-vals monkeys lvar)
        vars (vals mvars)
        root (mvars "root")]
    (run 1 [q]
         (== q vars)
         (and* (for [[name job] monkeys
                     :let [v (mvars name)]]
                 (if-let [n (:value job)]
                   (== v n)
                   (let [[op l r] (:eq job)
                         l (mvars l)
                         r (mvars r)]
                     (condp = op
                       "*" (fd/* l r v)
                       "/" (fd/* v r l)
                       "+" (fd/+ l r v)
                       "-" (fd/+ l v r))))))
         (everyg #(fd/in % (fd/interval Integer/MIN_VALUE Integer/MAX_VALUE)) vars))))

(->> "input21a"
     slurp
     s/split-lines
     (map read-monkey)
     (into {})
     monkeys)

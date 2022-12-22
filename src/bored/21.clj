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

(defn monkeys2 [monkeys part2]
  (let [mvars (into {} (map (fn [[k _]] [k (lvar)]) monkeys))]
    (run 1 [q]
         (== q {:human (mvars "humn")
                :root (mvars "root")})
         (and* (for [[name job] monkeys
                     :let [v (mvars name)]]
                 (let [[op l r] (:eq job)
                       l (mvars l)
                       r (mvars r)]
                   (cond
                     (and part2 (= name "root")) (== l r)
                     (and part2 (= name "humn")) (== 0 0)
                     (= "*" op) (fd/* l r v)
                     (= "/" op) (fd/* r v l)
                     (= "+" op) (fd/+ l r v)
                     (= "-" op) (fd/+ v r l)
                     (nil? op) (== v (:value job)))))))))

(let [data (->> "input21a"
                slurp
                s/split-lines
                (map read-monkey))]
  {:part1 (monkeys2 data false)
   :part2 (monkeys2 data true)})

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

(defn monkeys [monkeys]
  (let [mvars (into {} (map (fn [[k _]] [k (lvar)]) monkeys))]
    (run 1 [q]
         (== q (mvars "root"))
         (and* (for [[name job] monkeys
                     :let [v (mvars name)]]
                 (if-let [n (:value job)]
                   (== v n)
                   (let [[op l r] (:eq job)
                         l (mvars l)
                         r (mvars r)]
                     (condp = op
                       "*" (fd/* l r v)
                       "/" (fd/* r v l)
                       "+" (fd/+ l r v)
                       "-" (fd/+ v r l)))))))))

(defn monkeys2 [monkeys]
  (let [mvars (into {} (map (fn [[k _]] [k (lvar)]) monkeys))]
    (run 1 [q]
         (== q (mvars "humn"))
         (and* (for [[name job] monkeys
                     :let [v (mvars name)]]
                 (cond
                   (= name "root")
                   (let [[_ l r] (:eq job)]
                     (== (mvars l) (mvars r)))
                   (= name "humn") (== 0 0)
                   (:value job) (== v (:value job))
                   :else (let [[op l r] (:eq job)
                               l (mvars l)
                               r (mvars r)]
                           (condp = op
                             "*" (fd/* l r v)
                             "/" (fd/* r v l)
                             "+" (fd/+ l r v)
                             "-" (fd/+ v r l)))))))))

(->> "input21b"
     slurp
     s/split-lines
     (map read-monkey)
     monkeys2)

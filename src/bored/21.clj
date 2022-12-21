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
(read-monkey "rbzv:  gzpt - spng")

(defn  replace-vals [coll f]
  (into {} (map (fn [[k _]] [k (f)]) coll)))

(defn monkeys [monkeys]
  (let [mvars (replace-vals (into {} monkeys) lvar)]
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

(->> "input21b"
     slurp
     s/split-lines
     (map read-monkey)
     monkeys)

(defn monkeys-dumb [monkeys]
  (let [ms (into {} monkeys)
        root (ffirst monkeys)
        value (fn value [name]
                (let [job (ms name)]
                  (if-let [n (:value job)]
                    n
                    (let [[op l r] (:eq job)
                          op ({"*" *' "+" +' "/" / "-" -'} op)
                          l (value l)
                          r (value r)]
                      (op l r)))))]
    (map value (map first monkeys))))

(ns bored.core
  (:require [clojure.string :as s]))
(def val {:rock 1
          :paper 2
          :scissors 3})
(def  trans {"X" :rock "A" :rock
             "Y" :paper "B" :paper
             "Z" :scissors "C" :scissors})
(def wins {:rock :scissors
           :paper :rock
           :scissors :paper})
(defn line->score [l]
  (let [[opp my] (map trans (s/split l #" "))]
    (+ (my val) (cond (= opp my) 3
                      (= (wins my) opp) 6
                      :else 0))))
(->> "input"
     slurp
     s/split-lines
     (map line->score)
     (reduce +))

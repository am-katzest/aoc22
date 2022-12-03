(ns bored.core
  (:require [clojure.string :as s]))
temptation

(def val {:r 1
          :p 2
          :s 3})
(def trans {"X" :r "A" :r
            "Y" :p "B" :p
            "Z" :s "C" :s})
(def wins {:r :s
           :p :r
           :s :p})
(defn line->score [l]
  (let [[opp my] (map trans (s/split l #" "))]
    (+ (my val) (cond (= opp my) 3
                      (= (wins my) opp) 6
                      :else 0))))
(->> "input2"
     slurp
     s/split-lines
     (map line->score)
     (reduce +))

(def loses {:s :r
            :r :p
            :p :s})

(def trans2 {"X" wins "A" :r
             "Y" identity "B" :p
             "Z" loses "C" :s})
(defn line->score2 [l]
  (let [[opp strategy] (map trans2 (s/split l #" "))
        my (strategy opp)]
    (+ (my val) (cond (= opp my) 3
                      (= (wins my) opp) 6
                      :else 0))))
(->> "input2"
     slurp
     s/split-lines
     (map line->score2)
     (reduce +))

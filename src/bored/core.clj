(ns bored.core
  (:require [clojure.string :as s]))
(->> "input"
     slurp
     s/split-lines
     (partition-by #{""})
     (remove #{'("")})
     (map (fn [x]
            (->> x
                 (map #(Integer/parseInt %))
                 (reduce +))))
     (sort >)
     (take 3)
     (reduce +))

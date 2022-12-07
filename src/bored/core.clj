(ns bored.core
  (:require
   [clojure.string :as s]))

(defn cd? [l] (second (re-find #"\$ cd (.*)" l)))
(defn back? [x] (= "$ cd .." x))
(defn file? [l] (when-let [[_ size _] (re-matches #"([0-9]+) (.+)" l)]
                  (Integer/parseInt size)))

(defn sum-sizes [input]
  (loop [[l & r] input
         acc []
         sum 0]
    (cond
      (or (nil? l) (back? l)) [r acc sum]
      (cd? l) (let [[r a s] (sum-sizes r)
                    sum' (+ sum s)
                    acc' (concat acc a [s])]
                (if r
                  (recur r acc' sum')
                  [r acc' sum']))
      (file? l) (recur r acc (+ sum (file? l)))
      :else (recur r acc sum))))

(let [sizes
      (->> "input7b"
           slurp
           s/split-lines
           sum-sizes
           second)
      free-so-far (- 70000000 (apply max sizes))
      need-free (-  30000000  free-so-far)]
  {:first      (->> sizes
                    (filter #(>= 100000 %))
                    (reduce +))
   :second (->> sizes
                (filter #(<= need-free %))
                (apply min))})

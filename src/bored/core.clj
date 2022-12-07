(ns bored.core
  (:require
   [clojure.string :as s]))

(defn cd? [l] (second (re-find #"\$ cd (.*)" l)))
(defn back? [x] (= "$ cd .." x))
(defn file? [l] (when-let [[_ size _] (re-matches #"([0-9]+) (.+)" l)]
                  (Integer/parseInt size)))
(defn deltas [path size]
  (loop [acc {}
         path path]
    (if (empty? path) acc
        (recur (merge acc {path size}) (pop path)))))

(defn sum-sizes [input]
  (loop [[l & r] input
         acc {}
         path []]
    (cond
      (nil? l) acc
      (back? l) (recur r acc (pop path))
      (cd? l) (recur r acc (conj path (cd? l)))
      (file? l) (recur r (merge-with + acc (deltas path (file? l))) path)
      :else (recur r acc path))))

(let [sizes
      (->> "input7b"
           slurp
           s/split-lines
           sum-sizes
           (map second))
      free-so-far (- 70000000 (apply max sizes))
      need-free (-  30000000  free-so-far)]
  {:first      (->> sizes
                    (filter #(>= 100000 %))
                    (reduce +))
   :second (->> sizes
                (filter #(<= need-free %))
                (apply min))})

(ns bored.core
  (:require
   [clojure.string :as s]))

(defn cd? [l] (second (re-find #"\$ cd (.*)" l)))
(defn back? [x] (= "$ cd .." x))
(defn ls? [l] (re-matches #"\$ ls" l))
(defn dir? [l] (when-let [[_ d] (re-find #"dir (.+)" l)] {d {}}))
(defn file? [l] (when-let [[_ size name] (re-matches #"([0-9]+) (.+)" l)]
                  {name (Integer/parseInt size)}))

(defn make-hierarchy [input]
  (loop [[l & r] input
         acc {}
         path []]
    (cond
      (nil? l) acc
      (back? l) (recur r acc (pop path))
      (cd? l) (recur r acc (conj path (cd? l)))
      (ls? l) (recur r acc path)
      :else (recur r (update-in acc path merge
                                (or (file? l) (dir? l))) path))))

(defn sum-dir-sizes [map]
  (loop [[[k v] & r] (seq map)
         dirs []
         size 0]
    (cond
      (nil? v) [size dirs]
      (int? v) (recur r dirs (+ size v))
      :else (let [[s d] (sum-dir-sizes v)]
              (recur r (concat dirs d {k s}) (+ s size))))))

(let [[root rest] (->> "input7b"
                       slurp
                       s/split-lines
                       make-hierarchy
                       sum-dir-sizes)
      free-so-far (- 70000000 root)
      need-free (-  30000000  free-so-far)]
  (->> rest
       (map second)
       (filter #(<= need-free %))
       (apply min)))

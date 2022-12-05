(ns bored.core
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn transpose [m]
  (apply mapv vector m))

(defn read-stack [lines]
  (->> lines
       (map #(->> % (partition-all 4) (map second)))
       transpose
       (map #(drop-while #{\space} %))))

(defn read-instruction [line]
  (->> line
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn move-stack [stack [count si di]]
  (let [si (dec si)
        di (dec di)]
    (-> #(let [[h & t] (get % si)
               n (cons h (get % di))]
           (assoc %
                  si t
                  di n))
        (iterate stack)
        (nth count))))

(let [lines (->> "input5b" slurp s/split-lines)
      [a b] (split-with (complement #{""}) lines)
      stack (->> a reverse rest reverse read-stack vec)
      instructions (->> b rest (map read-instruction))]
  (->> instructions
       (reduce move-stack stack)
       (map first)
       (apply str)))

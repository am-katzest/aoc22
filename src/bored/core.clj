(ns bored.core
  (:require [clojure.string :as s]))

(defn read-stack [lines]
  (->> lines
       (map #(->> % (partition-all 4) (map second)))
       (apply mapv vector)
       (mapv #(drop-while #{\space} %))))

(defn read-instruction [line]
  (->> line
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn move-stack [stack [count si di]]
  (let [si (dec si)
        di (dec di)
        [h t] (split-at count (get stack si))
        n (concat (reverse h) (get stack di))]
    (assoc stack
           si t
           di n)))

(defn move-9001 [stack [count si di]]
  (let [si (dec si)
        di (dec di)
        [h t] (split-at count (get stack si))
        n (concat h (get stack di))]
    (assoc stack
           si t
           di n)))

(let [lines (->> "input5b" slurp s/split-lines)
      [a b] (split-with (complement #{""}) lines)
      stack (->> a reverse rest reverse read-stack)
      instructions (->> b rest (map read-instruction))]
  (->> instructions
       (reduce move-stack stack)
       (map first)
       (apply str)))

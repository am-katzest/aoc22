(ns bored.20
  (:require [clojure.string :as s]))

(defn remove-and-find-index [coll s]
  (loop [i 0
         [e & es] coll
         acc []]
    (if (= e s) [i (concat acc es)]
        (recur (inc i) es (conj acc e)))))

(defn insert-at [coll s idx]
  (loop [i 0
         [e & es] coll
         acc []]
    (if (= i idx) (concat acc (if e [s e] [s]) es)
        (recur (inc i) es (conj acc e)))))

(defn roll [coll command]
  (let [[_ v] command
        cnt (count coll)
        [idx coll'] (remove-and-find-index coll command)
        idx' (mod (+ idx v) (dec cnt))]
    (insert-at coll' command idx')))
(defn extract [coll]
  (->> coll
       (map second)
       cycle
       (drop-while #(not (zero? %)))
       (partition 1000)
       rest
       (map first)
       (take 3)
       (reduce +)))

(let [data (->> "input20a"
                slurp
                s/split-lines
                (map #(Integer/parseInt %)))
      data_1 (map-indexed (fn [i x] [i x]) data)
      data_2 (map-indexed (fn [i x] [i (* x 811589153)]) data)]
  {:part_1 (->> data_1
                (reduce roll data_1)
                extract)
   :part_2 (->>
            data_2
            (iterate #(reduce roll % data_2))
            (#(nth % 10))
            extract)})

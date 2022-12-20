(ns bored.20
  (:require [clojure.string :as s]))

(defn remove-and-find-index [coll s]
  (loop [i 0
         [e & es] coll
         acc []]
    (if (= e s) [i (concat acc es)]
        (recur (inc i) es (conj acc e)))))

(defn make-bi [xs]
  [(into {} xs)
   (into {} (map (fn [[a b]] [b a]) xs))])

(defn dissoc-key-bi [[k->v v->k] k]
  [(dissoc k->v k)
   (dissoc v->k (k->v k))])

(defn assoc-key-bi [[k->v v->k] k v]
  [(assoc k->v k v)
   (assoc v->k v k)])

(defn swap-keys-bi [[k->v v->k] a b]
  (let [va (k->v a)
        vb (k->v b)]
    [(assoc k->v a vb b va)
     (assoc v->k va b vb a)]))

(defn roll [ms command]
  (let [[i v] command
        cnt (count (first ms))          ; O(1)
        idx (get-in ms [1 command])
        idx' (mod (+ idx v) cnt)]
    (println v idx idx')
    (swap-keys-bi ms idx idx')))

(let [data (->> "input20a"
                slurp
                s/split-lines
                (map #(Integer/parseInt %))
                (map-indexed vector))
      ms (->>  data (map-indexed vector) make-bi)]
  (->> (reductions roll ms data)
       (map #(->> %
                  second
                  (sort-by second)
                  (map first)
                  (map second)))

       doall
       (mapv println)))

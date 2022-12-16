(ns bored.16
  (:require [clojure.string :as s]))

(defn read-node [l]
  (let [[_ name _ _ _ x _ _ _ _ & destinations] (s/split l #"[ ,=;]+")]
    [name {:val (Integer/parseInt x)
           :targets
           (into {} (map (fn [x] [x 1]) destinations))}]))

(defn prune [m nam]
  (println (type m) (type nam))
  (let [targets (:targets (m nam))]
    (reduce (fn [c [k v]]
              (let [replacements (->> k
                                      (dissoc targets)
                                      (map (fn [[i x]] [i (+ v x)]))
                                      (into {}))]
                (update-in c [k :targets]
                           (fn [lst] (-> (merge-with min lst replacements)
                                         (dissoc nam))))))
            (dissoc m nam) targets)))

(let [raw-nodes (->> "input16b"
                     slurp
                     s/split-lines
                     (map read-node)
                     (into {}))
      starting (ffirst raw-nodes)
      useless    (->> raw-nodes
                      (remove (fn [[name {:keys [val]}]]
                                (or (< 0 val) (= name starting))))
                      (map first))]
  (mapv println (reduce prune raw-nodes useless)))

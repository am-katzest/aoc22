(ns bored.16
  (:require [clojure.string :as s]))

(defn read-node [l]
  (let [[_ name _ _ _ x _ _ _ _ & destinations] (s/split l #"[ ,=;]+")]
    [name {:val (Integer/parseInt x)
           :targets
           (into {} (map (fn [x] [x 1]) destinations))}]))

(defn prune [m nam]
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

(defn complete [nodes]
  (let [all (into #{} (keys nodes))]
    (into {} (for [src all
                   :let [rest (disj all src)]]
               [src {:val (get-in nodes [src :val])
                     :targets (into {}
                                    (for [dst rest
                                          :let [rrest (disj rest dst)]
                                          :let [best (-> prune
                                                         (reduce nodes rrest)
                                                         (get-in [src :targets]))]] best))}]))))

(defn idk  [starting nodes]
  ((fn ik [name pool rem-time sum]
     (let [candidates
           (->> (for [x pool
                      :let [whole (nodes x)
                            value (:val whole)
                            d (inc (get-in whole [:targets name]))
                            t (- rem-time d)
                            cost-f (/ d rem-time)
                            sum (* value t)]
                      :when (>= t 0)]
                  [x  (int (/ sum cost-f)) d sum])
                (sort-by second >)
                ((fn [x] (println x) x))
                1)]
       (if (zero? (count candidates)) sum
           (apply max (for [[n _ c s] candidates]
                        (ik n (disj pool n) (- rem-time c) (+ sum s)))))))
   (first starting) (disj (set (keys nodes)) (first starting)) 30 0))

(let [raw-nodes (->> "input16a"
                     slurp
                     s/split-lines
                     (map read-node))
      starting (ffirst raw-nodes)
      useless    (->> raw-nodes
                      (remove (fn [[name {:keys [val]}]]
                                (or (< 0 val) (= name starting))))
                      (map first))
      nodes   (complete (reduce prune (into {} raw-nodes) useless))
      [starting-node] (filter (fn [[x _]] (= starting x)) nodes)]
  (idk starting-node nodes))

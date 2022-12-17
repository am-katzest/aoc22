(ns bored.16
  (:require [clojure.string :as s]))

(defn unnil [num]
  (let [a1 (int (/ num 26))
        a2 (mod num 26)]
    (apply str (map #(char (+ (int \A) %)) [a1 a2]))))
(def nilcopy (map unnil (map #(nth % 2) '((4 . 541) (8 . 132) (11 . 129) (16 . 253) (19 . 264) (22 . 225) (25 . 397) (29 . 213)))))
(def nilcopy2 (into {} (map-indexed (fn [i x] [x (- 100 i)]) nilcopy)))
(defn replay [n] (get nilcopy2 n 0))

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
     (println name (- 30 rem-time) sum)
     (let [candidates
           (->> (for [x pool
                      :let [whole (nodes x)
                            value (:val whole)
                            d (get-in whole [:targets name])
                            t (- rem-time d)
                            ;; cost-f (/ d rem-time)
                            sum (* value t)
                            heur (/ sum d)]
                      :when (pos? sum)]
                  [x (replay x) (dec t) sum])
                (sort-by second >)
                ((fn [x] (println x) x))
                (take 1))]
       (if (zero? (count candidates)) sum
           (apply max (for [[n _ t s] candidates]
                        (ik n (disj pool n) t (+ sum s)))))))
   starting (disj (set (keys nodes)) starting) 30 0))
(let [raw-nodes (->> "input16c"
                     slurp
                     s/split-lines
                     (map read-node))
      starting (ffirst raw-nodes)
      useless    (->> raw-nodes
                      (remove (fn [[name {:keys [val]}]]
                                (or (< 0 val) (= name starting))))
                      (map first))
      nodes   (complete (reduce prune (into {} raw-nodes) useless))]
  (time (idk starting nodes)))

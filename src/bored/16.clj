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

(defn complete [nodes]
  (->> (for [[x {:keys [val targets]}] nodes
             :let [ans (loop [neighbors (keys targets)
                              acc {x 0}
                              c 1]
                         (if (empty? neighbors) (dissoc acc x)
                             (let [acc' (into acc (map (fn [x] [x c]) neighbors))
                                   neighbors'
                                   (->> (for [x neighbors
                                              [xx _] (get-in nodes [x :targets])]
                                          xx)
                                        (remove acc')
                                        sort
                                        dedupe)]
                               (recur neighbors' acc' (inc c)))))]]
         [x {:val val :targets ans}])
       (into {})))

(defn best-path [current nodes avialable time acc]
  (let [whole (nodes current)
        gain (* time (:val whole))
        next-steps (for [[x d] (:targets whole)
                         :when (avialable x)
                         :let [time-to (inc d)]
                         :when (pos? (- time time-to))]
                     [(replay x) x time-to])
        ;; next-steps (take 1 (sort-by first > next-steps))
        acc' (+ acc gain)
        me [acc' current (- 30  time)]]
    (if (seq next-steps)
      (conj
       (apply max-key ffirst [[0]]
              (for [[_ x t] next-steps]
                (best-path x nodes (disj avialable x) (- time t) acc')))
       me)
      [me])))

(let [raw-nodes (->> "input16c"
                     slurp
                     s/split-lines
                     (map read-node))
      starting (ffirst raw-nodes)
      useless    (->> raw-nodes
                      (remove (fn [[name {:keys [val]}]]
                                (or (< 0 val) (= name starting))))
                      (map first))
      nodes   (apply dissoc (complete (into {} raw-nodes)) useless)]
  (best-path starting nodes (disj (into #{} (keys nodes)) starting) 30 0))

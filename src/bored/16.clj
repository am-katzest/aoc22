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

(defn idk  [starting nodes]
  ((fn ik [name pool rem-time sum]
     (let [children (->> (for [x pool
                               :let [node (nodes x)
                                     distance (get-in node [:targets name])
                                     rem-time' (- rem-time distance 1)
                                     total-gain (* (:val node) rem-time')]
                               :when (pos? total-gain)]
                           (ik x (disj pool x) rem-time' (+ sum total-gain))))]
       (concat (if (empty? children) []
                   (apply max-key ffirst children)) [[sum name rem-time]])))
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
      nodes   (apply dissoc (time (complete (into {} raw-nodes))) useless)]
  (idk starting nodes))

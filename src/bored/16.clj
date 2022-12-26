(ns bored.16
  (:require [clojure.string :as s]
            [clojure.set :as set]))

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

(defn best-path-heur [current nodes avialable time acc]
  (let [whole (nodes current)
        gain (* time (:val whole))
        next-steps (for [[x d] (:targets whole)
                         :when (avialable x)
                         :let [time-to (inc d)]
                         :when (pos? (- time time-to))
                         :let [val (get-in nodes [x :val])]]
                     [(/ (* val (- time time-to)) time-to) x time-to])
        tresh (->> next-steps
                   (map first)
                   (apply max 0)
                   (* 1/2))
        next-steps (filter #(> (first %) tresh) next-steps)
        acc' (+ acc gain)]
    (if (seq next-steps)
      (apply max acc'
             (for [[_ x t] next-steps]
               (best-path-heur x nodes (disj avialable x) (- time t) acc')))
      acc')))

(defn power [s]
  (loop [[f & r] (seq s) p '(())]
    (if f (recur r (concat p (map (partial cons f) p)))
        p)))

(defn power-set [s] (set (map set (power s)))) ;copied from SO

(defn gen-sets [s]
  (->> s
       power-set
       (map (fn [x] [x (set/difference s x)]))
       (filter (fn [[x y]] (pos? (compare (vec x) (vec y)))))))

(let [raw-nodes (->> "input16a"
                     slurp
                     s/split-lines
                     (map read-node))
      starting "AA"
      useless    (->> raw-nodes
                      (remove (fn [[name {:keys [val]}]]
                                (or (< 0 val) (= name starting))))
                      (map first))
      nodes   (apply dissoc (complete (into {} raw-nodes)) useless)
      avialable (disj (into #{} (keys nodes)) starting)]
  {:part1 (best-path-heur starting nodes  avialable  30 0)
   :part2 (->> avialable
               gen-sets
               (pmap  (fn [[x y]]
                        (+ (best-path-heur starting nodes x  26 0)
                           (best-path-heur starting nodes y  26 0))))
               (apply max))})

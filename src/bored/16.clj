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
  (let [c (count nodes)]
    (->> (for [[x {:keys [val targets]}] nodes
               :let [ans (loop [slns targets]
                           (if (= (dec c) (count slns)) slns
                               (recur (->>
                                       (for [[n d] slns
                                             :let [further (get-in nodes [n :targets])]
                                             [cand d2] further
                                             :when (not= cand x)
                                             :let [dt (+ d d2)
                                                   ex (targets cand)]
                                             :when (or (nil? ex) (> dt ex))]
                                         [cand dt])
                                       (into {})
                                       (merge-with min slns)))))]]
           [x {:val val :targets ans}])
         (into {}))))

(complete {"AA" {:val 1 :targets {"BB" 5 "DD" 1}}
           "BB" {:val 2 :targets {"AA" 5, "CC", 1}}
           "CC" {:val 3 :targets {"BB" 1, "DD", 2}}
           "DD" {:val 4 :targets {"CC" 2 "AA" 1}}})

(defn idk  [starting nodes]
  ((fn ik [name pool rem-time sum]
     ;; (println name (- 30 rem-time) sum)
     (let [candidates
           (->> (for [x pool
                      :let [whole (nodes x)
                            value (:val whole)
                            d (get-in whole [:targets name])
                            t (- rem-time d)
                            ;; cost-f (/ d rem-time)
                            sum (* value t)
                            ;; heur (/ sum d)
                            ]
                      :when (pos? sum)]
                  [x nil (dec t) sum]))]
       (if (zero? (count candidates)) sum
           (reduce max (for [[n _ t s] candidates]
                         (ik n (disj pool n) t (+ sum s)))))))
   starting (disj (set (keys nodes)) starting) 29 0))

(let [raw-nodes (->> "input16a"
                     slurp
                     s/split-lines
                     (map read-node))
      starting (ffirst raw-nodes)
      useless    (->> raw-nodes
                      (remove (fn [[name {:keys [val]}]]
                                (or (< 0 val) (= name starting))))
                      (map first))
      nodes   (apply dissoc (complete (into {} raw-nodes)) useless)]
  (println "done")
  (println (idk starting  nodes)))

(ns bored.12
  (:require [clojure.string :as s]
            [bored.heat :as h]))

(defn V+ [[a A] [b B]] [(+ a b) (+ A B)])

(defn V- [[a A] [b B]] [(+ a b) (+ A B)])

(defn Vabs [[x y]] [(if (neg? x) (- x) x)
                    (if (neg? y) (- y) y)])
(defn manhattan [a b]
  (->> (V- a b) (Vabs) (apply +)))

(defn index-of2 [arr search]
  (for [[x row] (map-indexed vector arr)
        [y val] (map-indexed vector row)
        :when (= search val)]
    [x y]))

(def ^:dynamic terrain)
(def ^:dynamic start)
(def ^:dynamic end)

(defrecord trail [current val history comp])

(defn make-children [visited {:keys [current val history]}]
  (for [dir [[0 1] [0 -1] [1 0] [-1 0]]
        :let [new-loc (V+ current dir)]
        :when (not (visited new-loc))
        :let [new-val (get-in terrain new-loc)]
        :when (some? new-val)
        :when (>= (- val new-val) -1)
        :let [g (count history)
              h (manhattan end new-loc)]]
    (->trail new-loc new-val (conj history current) [(+ g h) g new-loc])))

(defn pathfind []
  (let [starting (->trail start (get-in terrain start) [] [])
        todo (sorted-set-by #(compare (:comp %1) (:comp %2)))]
    (loop [todos (conj todo starting)
           visited  (transient #{start})]
      (when-let [choosen (first todos)]
        (if (= end (:current choosen)) choosen
            (let [children (make-children visited choosen)]
              (recur (-> todos
                         (disj choosen)
                         (into children))
                     (reduce conj! visited (map :current children)))))))))

(time (let [raw-data (->> "input12b"
                          slurp
                          (s/split-lines)
                          (mapv #(mapv int %)))
            [start-idx] (index-of2 raw-data (int \S))
            [end-idx] (index-of2 raw-data (int \E))
            data (-> raw-data
                     (assoc-in start-idx (int \a))
                     (assoc-in end-idx (int \z)))
            calc  #(binding [terrain data
                             start %
                             end end-idx]
                     (count (:history (pathfind))))
            candidates (index-of2 data (int \a)) ;; (for [i (range 41)] [i 0])
            ]
        {:first (calc start-idx)
         :second (apply min (remove zero? (pmap calc candidates)))}))

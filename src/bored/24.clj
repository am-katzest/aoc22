(ns bored.24
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn V+ [[a A] [b B]] [(+ a b) (+ A B)])

(defn V- [[a A] [b B]] [(+ a b) (+ A B)])

(defn Vabs [[x y]] [(if (neg? x) (- x) x) (if (neg? y) (- y) y)])

(defn manhattan [a b]
  (->> (V- a b) (Vabs) (apply +)))

(def ^:dynamic terrain)
(def ^:dynamic start)
(def ^:dynamic end)

(defn read-terr [coll]
  (into {:y (dec (count coll)) :x (dec (count (first coll)))}
        (for [[y line] (map-indexed vector coll)
              [x cell] (map-indexed vector line)
              :let [c [x y]
                    v (cond (and (= y 0) (= cell \.)) [:start c]
                            (and (= y (dec (count coll))) (= cell \.)) [:end c]
                            (= cell \.) nil
                            :else [[c cell] cell])]
              :when (some? v)]
          v)))
(defn advance [coll]
  (let [dirs {\v [0 1] \< [-1 0] \> [1 0] \^ [0 -1]}
        roll-1 (fn [max x] (cond (= 0 x) (dec max)
                                 (= max x) 1
                                 :else x))
        roll (fn [[x y]]
               [(roll-1 (:x coll) x)
                (roll-1 (:y coll) y)])
        moveable? (comp dirs second)
        m (filter moveable? coll)
        rest (into {} (remove moveable? coll))
        moved (map (fn [[[pos _] v]] [[(roll (V+ pos (dirs v))) v] v]) m)]
    (into rest moved)))

(defn grab-walls [coll]
  (->> coll
       (keep (fn [[k v]]
               (when (char? v)
                 (first k))))
       (into #{})))

(defrecord trail [location time comp])

(defn make-children [{:keys [location time]}]
  (let [time' (inc time)
        blocked (terrain time')]
    (for [dir [[0 1] [0 -1] [1 0] [-1 0] [0 0]]
          :let [loc' (V+ location dir)]
          :when (not (blocked loc'))
          :let [g time'
                h (manhattan end loc')]]
      (->trail loc' time' [(+ g h) g loc' time']))))

(defn pathfind [s e start-time]
  (binding [start s
            end e]
    (let [starting (->trail start start-time [])
          todo (sorted-set-by #(compare (:comp %1) (:comp %2)))]
      (loop [todos (conj todo starting)]
        (when-let [choosen (first todos)]
          (if (= end (:location choosen)) (:time choosen)
              (let [children (make-children choosen)]
                (recur (-> todos
                           (disj choosen)
                           (into children))))))))))

(defonce advance' (memoize advance))
(defonce grab-walls' (memoize grab-walls))

(defn print-terr [coll]
  (println)
  (println)
  (let [ys (sort (map first coll))
        xs (sort (map second coll))]
    (doseq [x (range (first xs)
                     (inc (last xs)))]
      (println)
      (doseq [y (range (first ys) (inc (last ys)))]
        (print (if (coll [y x])  "{}" "  "))))))

(let [init (->> "input24a"
                slurp
                s/split-lines
                read-terr)
      init (assoc init [(V+ (:start init) [0 -1]) \#] \#)
      init (assoc init [(V+ (:end init) [0 1]) \#] \#)
      terr (->>
            init
            (iterate advance')
            (map grab-walls')
            (take 5000)
            (into []))
      s (:start init)
      e (:end init)]
  (time (binding [terrain terr]
          (let [fst (pathfind s e 0)]
            {:part1 fst
             :part2 (->> fst
                         (pathfind e s)
                         (pathfind s e))}))))

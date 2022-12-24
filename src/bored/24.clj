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
  (into {:y (count coll) :x (count (first coll))}
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
  (let [dirs {\v [0 -1] \< [-1 0] \> [1 0] \^ [0 1]}
        roll (fn [[x y]]
               [(mod x (:x coll))
                (mod y (:y coll))])
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

(defn make-children [visited {:keys [location time]}]
  (let [time' (inc time)
        blocked (terrain time')]
    (for [dir [[0 1] [0 -1] [1 0] [-1 0]]
          :let [loc' (V+ location dir)]
          :when (not (blocked loc'))
          :let [g time'
                h (manhattan end loc')]]
      (->trail loc' time' [(+ g h) g loc' time']))))

(defn pathfind []
  (let [starting (->trail start 0 [])
        todo (sorted-set-by #(compare (:comp %1) (:comp %2)))]
    (loop [todos (conj todo starting)
           visited  (transient #{[start 0]})]
      (when-let [choosen (first todos)]
        (println choosen)
        (if (= end (:location choosen)) choosen
            (let [children (make-children visited choosen)]
              (recur (-> todos
                         (disj choosen)
                         (into children))
                     (reduce conj! visited (map (fn [c] [(:location c) (:time c)]) children)))))))))

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
(let [init (->> "input24aa"
                slurp
                s/split-lines
                read-terr)
      init (assoc init [(V+ (:start init) [0 -1]) \#] \#)
      terr (->>
            init
            (iterate advance')
            (map grab-walls')
            (take 5000)
            (into [])
            time)]
  (binding [terrain terr
            start (:start init)
            end (:end init)]
    (print-terr (terrain 2))))

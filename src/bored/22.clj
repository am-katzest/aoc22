(ns bored.22
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def dir->pretty {0 :right
                  1 :down
                  2 :left
                  3 :up})
(def dir->delta {0 [1 0]
                 1 [0 -1]
                 2 [-1 0]
                 3 [0 1]})
(def l->angle {\R -1 \L 1})

(defn turn [[loc direction] change]
  [loc (mod (+ direction (l->angle change)) 4)])

(defn read-row [i ss]
  (for [[j s] (map-indexed vector ss)
        :when (not= s \space)]
    [[(inc j) (inc i)] s]))
(println "restart")
(defn read-instructions [instructions]
  (->> instructions
       (partition-by #{\R \L})
       (map (fn [xs] (if (#{\R \L} (first xs))
                       (first xs)
                       (Integer/parseInt (apply str xs)))))))
(def ^:dynamic terrain)

(defn roll [[coords heading]]
  [coords heading])

(defn forward [[[x y] dir]]
  (let [[dx dy] (dir->delta dir)]
    [[(+ x dx) (+ y dy)] dir]))

(defn can-move? [[coords _]]
  (= \. (terrain coords)))

(defn inside-wall? [[coords _]]
  (= \# (terrain coords)))

(defn walk [pos [i & is]]
  (println pos)
  (cond
    (nil? i) pos
    (#{\R \L} i) (recur (turn pos i) is)
    (zero? i) (recur pos is)
    :else (let [pos' (roll (forward pos))]
            (if (inside-wall? pos') (recur pos is)
                (recur pos' (cons (dec i) is))))))
;; [[x y] dir]

(let [[terrain-lines _ [ins-line]] (->> "input22a"
                                        slurp
                                        s/split-lines
                                        (partition-by #{""}))
      terr (->> terrain-lines
                (map-indexed read-row)
                (reduce concat)
                (into {}))
      ins (read-instructions ins-line)
      leftupmost [(->> terr
                       (filter (fn [[[x y] v]] (and (= y 1) (= v \.))))
                       (sort-by ffirst)
                       ffirst) 0]]
  (binding [terrain terr]
    (walk leftupmost ins)))

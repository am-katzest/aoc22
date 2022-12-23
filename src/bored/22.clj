(ns bored.22
  (:require [clojure.string :as s]))

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

(defn read-instructions [instructions]
  (->> instructions
       (partition-by #{\R \L})
       (map (fn [xs] (if (#{\R \L} (first xs))
                       (first xs)
                       (Integer/parseInt (apply str xs)))))))

(def ^:dynamic terrain)

(defn outside-map? [coords & _]
  (nil? (terrain coords)))

(defn ^:dynamic wrap-around [[coords heading]]
  (let [delta (dir->delta heading)]
    (loop [c (mapv - coords delta)]
      (if (outside-map? c)
        [(mapv + c delta) heading]
        (recur (mapv - c delta))))))

(defn roll [[coords heading]]
  (if (outside-map? coords)
    (wrap-around [coords heading])
    [coords heading]))

(defn forward [[[x y] dir]]
  (let [[dx dy] (dir->delta dir)]
    [[(+ x dx) (+ y dy)] dir]))

(defn inside-wall? [[coords _]]
  (= \# (terrain coords)))

(defn walk [pos [i & is]]
  (cond
    (nil? i) pos
    (#{\R \L} i) (recur (turn pos i) is)
    (zero? i) (recur pos is)
    :else (let [pos' (roll (forward pos))]
            (if (inside-wall? pos') (recur pos is)
                (recur pos' (cons (dec i) is))))))

(defn password [[[x y] dir]]
  (+ dir (* 4 x) (* 1000 y)))

(let [[terrain-lines _ [ins-line]] (->> "input22b"
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
    (password (walk leftupmost ins))))

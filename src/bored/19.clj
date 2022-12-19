(ns bored.19
  (:require [clojure.string :as s]))

(defn read-ints [line]
  (->> (s/split line #"[^0-9-]+")
       (remove #{""})
       (map #(Integer/parseInt %))))

(defn make-blueprint [[id ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian]]
  {:id id
   :bots {:o {:o ore-ore}
          :c {:o clay-ore}
          :b {:o obsidian-ore
              :c obsidian-clay}
          :g {:o geode-ore
              :b geode-obsidian}}})

(defn has-enough? [resources cost]
  (if-let [[k v] (first cost)]
    (when (>= (k resources) v)
      (recur (update resources k - v) (rest cost)))
    resources))

(defn very-naive [blueprints]
  (let [botprints (:bots blueprints)]
    (loop [todo (into clojure.lang.PersistentQueue/EMPTY [[{:o 0 :g 0 :c 0 :b 0}
                                                           {:o 1 :g 0 :c 0 :b 0} 0]])]
      (if (empty? todo)
        (let [current (peek todo)
              [resources bots m] current
              children (if (= m 24) ()
                           (let [resources' (merge-with + resources bots)
                                 minute' (inc m)]
                             (cons [resources' bots minute']
                                   (for [[bot cost] blueprints
                                         :let [resources'' (has-enough? resources' cost)]
                                         :when (some? resources'')
                                         :let [bots' (update bots bot inc)]]
                                     [resources'' bots' minute']))))]
          (recur (->> children (into (pop todo)))))))))
(defn naive [blueprints orders]
  (let [blueprints (:bots blueprints)]
    (loop [resources {:o 0 :g 0 :c 0 :b 0}
           bots {:o 1 :g 0 :c 0 :b 0}
           m 0
           [bot & rest :as orders] orders]
      (if (>= m 24) [resources orders]
          (let [m' (inc m)]
            (if-let [resources' (and bot (has-enough? resources (blueprints bot)))]
              (recur (merge-with + resources' bots) (update bots bot inc) m' rest)
              (recur (merge-with + resources bots) bots m' orders)))))))

(def b1 (->> "input19a"
             slurp
             (s/split-lines)
             (map (comp make-blueprint read-ints))
             first))
(defn make-initial-guess [{:keys [c b g o]}]
  (let [obs-ratio (/ (:o b) (:c b))
        max-ore (max (:o c) (:o b) (:o g))
        target-clay (int (/ (:c b) 3))
        target-ore (int (/ max-ore 2))]
    [obs-ratio max-ore target-clay]
    (concat (repeat target-ore :o)
            (repeat target-clay :c)
            (repeat 2 :b)
            (repeat 2 :g))))

(make-initial-guess (:bots b1))

(naive b1 [:c :c :c :b :c :b :g :g])

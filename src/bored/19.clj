(ns bored.19
  (:require [clojure.string :as s]
            [better-cond.core :as b]))

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

(defn calc-maxes
  "calculates maximum production we want of each resource type"
  [{:keys [c b g]}]
  {:o (max (:o c) (:o b) (:o g))
   :c (:c b)
   :b (:b g)})

(defn  avialable-paths [gain botprints maxes]
  (let [produced? #(or (nil? %) (pos? (% gain)))
        need-more? #(< (% gain) (% maxes))]
    (for [[res req] [[:o] [:c] [:b :c]]
          :when (produced? req)
          :when (need-more? res)] res)))

(defn has-enough? "to make some machine" [resources [cost & costs]]
  (if-let [[k v] cost]
    (when (>= (k resources) v)
      (recur (update resources k - v) costs))
    resources))

(defn semi-naive-turn [blueprints  resources bots turn chosen maxes]
  (b/cond (= turn 27) (:g resources)
          :let [after-g (has-enough? resources (:g blueprints))]
          ;; can built geode
          (some? after-g) (recur blueprints
                                 (merge-with + after-g bots)
                                 (update bots :g inc)
                                 (inc turn) chosen maxes)
          :let [after-c (has-enough? resources (chosen blueprints))]
          ;; cannot build anything
          (nil? after-c) (recur blueprints (merge-with + resources bots) bots (inc turn) chosen maxes)
          :let [resources' (merge-with + after-g bots)
                bots' (update bots chosen inc)]
          (apply max 0
                 (for [choice (avialable-paths bots' blueprints maxes)]
                   (semi-naive-turn blueprints resources' bots' (inc turn) choice maxes)))))

(defn semi-naive-launch [blueprints]
  (let [blueprints (:bots blueprints)
        resources {:o 0 :g 0 :c 0 :b 0}
        bots {:o 1 :g 0 :c 0 :b 0}
        maxes (calc-maxes blueprints)]
    (max (semi-naive-turn blueprints resources bots 0 :o maxes)
         (semi-naive-turn blueprints resources bots 0 :c maxes))))

(->> "input19a"
     slurp
     (s/split-lines)
     (map (comp make-blueprint read-ints))
     first
     semi-naive-launch
     time)

;; (make-initial-guess (:bots b1))

;; (naive b1 [:c :c :c :b :c :b :g :g])

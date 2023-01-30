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
    (for [[res req] [[:b :c] [:o] [:c]]
          :when (produced? req)
          :when (need-more? res)] res)))

(defn has-enough? "to make some machine" [resources [cost & costs]]
  (if-let [[k v] cost]
    (when (>= (k resources) v)
      (recur (update resources k - v) costs))
    resources))

(defn semi-naive [blueprints]
  (let [id (:id blueprints)
        blueprints (:bots blueprints)
        resources {:o 0 :g 0 :c 0 :b 0}
        bots {:o 1 :g 0 :c 0 :b 0}
        maxes (calc-maxes blueprints)
        turn (fn semi-naive-turn [resources bots turn chosen]
               (b/cond (= turn 24) (+ (:g bots) (:g resources))
                       :let [mine #(merge-with + bots %2)
                             add-bot #(update bots %2 inc)
                             after-g (has-enough? resources (:g blueprints))]
                       ;; can built geode
                       (some? after-g) (recur (mine after-g) (add-bot :g) (inc turn) chosen)
                       :let [after-c (has-enough? resources (chosen blueprints))]
                       ;; cannot build anything
                       (nil? after-c) (recur (mine resources) bots (inc turn) chosen)
                       :let [resources' (mine after-c)
                             bots' (add-bot chosen)]
                       (apply max 0
                              (for [choice (avialable-paths bots' blueprints maxes)]
                                (semi-naive-turn  resources' bots' (inc turn) choice)))))]
    [id (max (turn resources bots 0 :o)
             (turn resources bots 0 :c))]))

(->> "input19a"
     slurp
     (s/split-lines)
     (map (comp semi-naive make-blueprint read-ints)))

;; (make-initial-guess (:bots b1))

;; (naive b1 [:c :c :c :b :c :b :g :g])

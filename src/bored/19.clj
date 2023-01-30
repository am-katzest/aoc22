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
   :b (:b g)
   :g 999})

(defn  avialable-paths [gain botprints maxes]
  (let [produced? #(or (nil? %) (pos? (% gain)))
        need-more? #(< (% gain) (% maxes))]
    (for [[res req] [[:g :b] [:b :c] [:o] [:c]]
          :when (produced? req)
          :when (need-more? res)] res)))

(defn has-enough? "to make some machine" [resources [cost & costs]]
  (if-let [[k v] cost]
    (when (>= (k resources) v)
      (recur (update resources k - v) costs))
    resources))

(defn semi-naive [time blueprints]
  (let [id (:id blueprints)
        blueprints (:bots blueprints)
        resources {:o 0 :g 0 :c 0 :b 0}
        bots {:o 1 :g 0 :c 0 :b 0}
        maxes (calc-maxes blueprints)
        turn (fn semi-naive-turn [resources bots turn chosen]
               (b/cond (= turn  time) (+ (:g bots) (:g resources))
                       :let [mine #(merge-with + bots %)
                             add-bot #(update bots % inc)]
                       :let [after-c (has-enough? resources (chosen blueprints))]
                       ;; cannot build anything
                       (nil? after-c) (recur (mine resources) bots (inc turn) chosen)
                       :let [resources' (mine after-c)
                             bots' (add-bot chosen)]
                       (apply max 0
                              (for [choice (avialable-paths bots' blueprints maxes)]
                                (semi-naive-turn  resources' bots' (inc turn) choice)))))
        result    (max (turn resources bots 1 :o)
                       (turn resources bots 1 :c))]
    [id result]))

(let [data (-> "input19b" slurp s/split-lines)]
  {:part1 (->> data
               (pmap #(->> % read-ints
                           make-blueprint
                           (semi-naive 24)
                           (apply *)))
               (reduce +))
   :part2 (->> data
               (take 3)
               (pmap #(->> % read-ints
                           make-blueprint
                           (semi-naive 32)
                           second))
               (apply *))})

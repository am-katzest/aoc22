(ns bored.11
  (:require
   [clojure.string :as s]))

(defn read-ints [line]
  (->> (s/split line #"[^0-9-]+")
       (remove #{""})
       (map #(Integer/parseInt %))))

(def first-int (comp first read-ints))

(defn make-op [[_ _ _ _ a op b]]
  (let [a (first-int a)
        b (first-int b)
        op ({"*" *', "+" +'} op)]
    (fn [old] (op (or a old) (or b old)))))

(defn read-monkey [[_ starting op test if-true if-false _]]
  (let [t (first-int if-true)
        f (first-int if-false)
        test (first-int test)]
    {:items (vec (read-ints starting))
     :op (make-op (s/split op #" +"))
     :div test
     :where? #(if (zero? (mod % test)) t f)
     :inspected 0}))

(defn ^:dynamic relief [x] (bigint (/ x 3)))

(defn- run-monkey [ms i]
  (let [{:keys [op where? items]} (ms i)]
    (-> (loop [[v & t] items ms ms]
          (if-not v ms
                  (let [v' (-> v op relief)]
                    (recur t (update-in ms [(where? v') :items] conj v')))))
        (update-in [i :inspected] +' (count items))
        (assoc-in  [i :items] []))))

(defn round [ms]
  (loop [ms ms i 0]
    (if (= i (count ms)) ms
        (recur (run-monkey ms i) (inc i)))))

(defn GCD [a b]
  (cond (= a b) a
        (> b a) (recur b a)
        :else (recur b (- a b))))

(defn LCM [a b] (/  (* a b) (GCD a b)))

(defn calc-business [ms cnt]
  (->> (nth (iterate round ms) cnt)
       (map :inspected) (sort >) (take 2) (apply *)))

(let [monkeys
      (->> "input11b"
           slurp
           (s/split-lines)
           (partition-all 7)
           (mapv read-monkey))
      lcm (->> monkeys (map :div) (reduce LCM))]
  {:part1 (calc-business monkeys 20)
   :part2 (binding [relief #(mod % lcm)]
            (calc-business monkeys 10000))})

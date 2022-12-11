(ns bored.11
  (:require
   [clojure.string :as s]))

(defn read-int [x] (Integer/parseInt x))

(defn read-ints [line] (->> (s/split line #"[^0-9-]+") (remove #{""}) (map read-int)))

(defn make-op [[_ _ _ _ a op b]]
  (fn [old] (let [op ({"*" *', "+" +'} op)
                  or-old #(if (= "old" %) old (read-int %))]
              (op (or-old a) (or-old b)))))

(defn read-monkey [[id starting op test ift iff _]]
  (let [t (first (read-ints ift))
        f (first (read-ints iff))
        test (first (read-ints test))]
    {:id (first (read-ints id))
     :items (vec (read-ints starting))
     :op (make-op (s/split op #" +"))
     :div test
     :where? #(if (zero? (mod % test)) t f)
     :inspected 0}))

(defn ^:dynamic relief [x] (bigint (/ x 3)))

(defn- run-monkey [ms i]
  (let [m (ms i)
        {:keys [op where? items]} m
        ms' (loop [[v & t] items ms ms]
              (if v
                (let [v' (-> v op relief)
                      target-m (where? v')]
                  (recur t (update-in ms [target-m :items] conj v')))
                ms))]
    (-> ms'
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

(defn calc-business [ms]
  (->> ms (map :inspected) (sort >) (take 2) (apply *)))

(let [monkeys
      (->> "input11b"
           slurp
           (s/split-lines)
           (partition-all 7)
           (mapv read-monkey))
      lcm (->> monkeys (map :div) (reduce LCM))]
  {:part1 (->> (nth (iterate round monkeys) 20) calc-business)
   :part2 (binding [relief #(mod % lcm)]
            (->> (nth (iterate round monkeys) 10000) calc-business))})

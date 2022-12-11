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
     :where? #(if (zero? (mod % test)) t f)
     :inspected 0}))

(defn relief [x] (bigint (/ x 3)))

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
        (update-in [i :inspected] + (count items))
        (assoc-in  [i :items] []))))

(defn round [ms]
  (loop [ms ms i 0]
    (if (= i (count ms)) ms
        (recur (run-monkey ms i) (inc i)))))

(let [monkeys
      (->> "input11b"
           slurp
           (s/split-lines)
           (partition-all 7)
           (mapv read-monkey))
      last (nth (iterate round monkeys) 20)]
  (->> last (map :inspected) (sort >) (take 2) (reduce *')))

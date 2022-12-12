(ns bored.12
  (:require [clojure.string :as s]
            [bored.heat :as h]))

(defn V
  "applies operator to each element of vectors"
  [op & vecs] (apply mapv op vecs))

(defn abs [x] (if (neg? x) (- x) x))

(defn manhattan [a b]
  (->> (V - a b) (V abs) (apply +)))

(defn index-of2 [arr search]
  (for [[x row] (map-indexed vector arr)
        [y val] (map-indexed vector row)
        :when (= search val)]
    [x y]))

(def ^:dynamic terrain)
(def ^:dynamic start)
(def ^:dynamic end)

;┌────┐
;│ A* │
;└────┘
(defrecord trail [current val history])

(defmacro by [f g & a]
  (cons f (map (fn [x] `(~g ~x)) a)))

(defn heur [x]
  (+ (manhattan (:current x) end)
     (count (:history x))))

;; (defn A*-ordering [a b]
;;   (if (by = :current a b) 0
;;       (let [A* (by compare heur a b)]
;;         (if-not (zero? A*) A*
;;                 (let [dst (by compare #(manhattan (:current %) end) b a)]
;;                   (if-not (zero? dst) dst (by compare :current a b)))))))
(defn A*-ordering [^trail a ^trail b]
  (if (by = :current a b) 0
      (let [A* (by compare heur a b)]
        (if-not (zero? A*) A*
                (by compare :current a b)))))

(defn make-child [{:keys [current val history]} direction]
  (let [new-position (V + current direction)]
    (when-let [new-val (get-in terrain new-position)]
      (let [where-we-came-from (peek history)
            height-diff (- val new-val)]
        (when (and (not= new-position where-we-came-from)
                   (>= height-diff -1))
          (->trail new-position new-val (conj history current)))))))

(def directions [[0 1] [0 -1] [1 0] [-1 0]])

(defn make-children [visited x]
  (filterv #(and (some? %) (not (visited (:current x))))
           (mapv #(make-child x %) directions)))

(defn pathfind []
  (let [starting (->trail start (get-in terrain start) [])
        todo (sorted-set-by A*-ordering)]
    (loop [todos (conj todo starting)
           visited #{}]
      (let [choosen (first todos)]
        (cond (= end (:current choosen)) choosen
              (nil? choosen) nil
              :else (recur (-> todos
                               (disj choosen)
                               (into (make-children visited choosen)))
                           (conj visited (:current choosen))))))))

(let [raw-data (->> "input12b"
                    slurp
                    (s/split-lines)
                    (mapv #(mapv int %)))
      [start-idx] (index-of2 raw-data (int \S))
      [end-idx] (index-of2 raw-data (int \E))
      data (-> raw-data
               (assoc-in start-idx (int \a))
               (assoc-in end-idx (int \z)))
      as (sort-by #(manhattan end-idx %) (index-of2 data (int \a)))]
  (binding [terrain data
            start start-idx
            end end-idx]
    (let [ans (pathfind)]
      (h/show-path data (:history ans))
      (println (count (:history ans)))))
  (comment (doseq [loc as]
             (binding [terrain data
                       start loc
                       end end-idx]
               (let [h (pathfind)]
                 (if h (println "\n" (count (:history h)))
                     (do (print "#") (flush))))))))

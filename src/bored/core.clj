(ns bored.core
  (:require [clojure.string :as str]))
(defmacro by [function mapping & xs]
  `(~function ~@(map (partial list mapping) xs)))

(defn fill-blank [s] (if (get-in s [:mem (:dp s)]) s
                         (assoc-in s [:mem (:dp s)] 0)))
(defn roll [x] (mod x 256))
(defn inc-ip [s] (update-in s [:ip] inc))
(defn change-mem [f] #(update-in (fill-blank %) [:mem (:dp %)] (comp roll f)))

(defn output [s] (update-in s [:output] #(cons (get-in s [:mem (:dp s)] 0) %)))
(defn input [s] {:pre [(seq (:input s))]}
  (-> s (assoc-in [:mem (:dp s)] (first (:input s)))
      (update :input rest)))

(defn seek [state dir]
  (letfn [(co [char] (get {\[ 1 \] -1} char 0))]
    (loop [s state off 0]
      (let [off' (+ off (co  (get-in s [:tape (:ip s)])))]
        (if (= off' 0) s
            (recur (update s :ip dir) off'))))))

(defn forward [s] (if (zero? (get-in s [:mem (:dp s)] 0)) (seek s inc) s))
(defn backward [s] (if (zero? (get-in s [:mem (:dp s)] 0)) s (seek s dec)))

(defn dec-dp [s] (update-in s [:dp] dec))
(def commands {\> #(update % :dp inc)
               \< #(update % :dp dec)
               \+ (change-mem inc)
               \- (change-mem dec)
               \. output
               \, input
               \[ forward
               \] backward})
(defn iter [s] (inc-ip ((get commands (get-in s [:tape (:ip s)])) s)))
(defn on-tape? [s] (< (:ip s) (.length (:tape s))))

(defn brute-reverse [f xs]
  (let [mem (->> xs (map (fn [x] {(f x) x})) (into {}))] #(get mem %)))
(def unchar (brute-reverse char (range 256)))
(defn execute-string [source input]
  (try (->> {:ip 0 :dp 0 :mem {} :tape source :input (map unchar input)}
            (iterate iter)
            (drop-while on-tape?)
            first
            :output
            reverse
            (map char)
            (apply str))
       (catch AssertionError _ nil)))

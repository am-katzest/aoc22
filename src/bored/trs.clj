(ns bored.trs
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic  :as l]
            [clojure.core.logic.arithmetic :as ar]
            [clojure.core.logic.nominal :as nom]
            [clojure.core.logic.fd :as fd]))
(defn cso [h t x]
  (conde [(firsto x h)
          (resto x t)]))

(defn sumo [rem [x & xs]]
  (if (nil? x) (== rem 0)
      (fresh [p]
             (fd/+ x p rem)
             (sumo p xs))))

(run* [q p]
      (fd/in q p (fd/interval 0 7))
      (sumo 5 [q p]))
(def trans (partial apply mapv vector))

(defn magic-squares [len sum]
  (let [vars (repeatedly (* len len) lvar)
        table (partition len vars)]
    (run* [q]
          (l/and* (map #(fd/in % (fd/interval 0 sum)) vars))
          (l/and* (map (fn [xs] (sumo sum xs)) table))
          (l/and* (map (fn [xs] (sumo sum xs)) (trans table)))
          (== q table))))
(defn always [] (conde [s#]
                       [(always)]))
(defn never [] (conde [(never)]))

(defne nonmembero
  "A relation where l is a collection, such that l does not contain x"
  [x l]
  ([_ ()])
  ([_ [head . tail]]
   (!= x head)
   (nonmembero x tail)))

(defn all-wrong [[a b c] q]
  (conde [(nonmembero a q)
          (nonmembero b q)
          (nonmembero c q)]))

(defmacro all-other-pos [keys keys2 sym sym2 one other]
  (cons `conde (map (fn [choosen]
                      (mapv (fn [key key2] `(let [~sym ~key ~sym2 ~key2]
                                              ~(if (= key choosen) one other))) keys keys2)) keys)))

(defn one-right-wrong-place [[a b c] [A B C]]
  (all-other-pos [a b c] [A B C] digit place
                 (conde [(membero digit [A B C])
                         (!= digit place)])
                 (nonmembero digit [A B C])))

(defn two-right-wrong-place [[a b c] [A B C]]
  (all-other-pos [a b c] [A B C] digit place
                 (nonmembero digit [A B C])
                 (conde [(membero digit [A B C])
                         (!= digit place)])))

(defn one-right-in-place [[a b c] [A B C]]
  (all-other-pos [a b c] [A B C] digit place
                 (== digit place)
                 (nonmembero digit [A B C])))

(run* [q]
      (fresh [a b c]
             (== q [a b c])
             (one-right-in-place [6 8 2] [a b c])
             (one-right-wrong-place [6 1 4] [a b c])
             (two-right-wrong-place [2 0 6] [a b c])
        ;; (all-wrong [7 3 8] [a b c])
        ;; (one-right-wrong-place [3 8 0] [a b c])
             ))

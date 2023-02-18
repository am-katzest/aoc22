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


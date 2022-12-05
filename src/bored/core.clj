(ns bored.core
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn  contain? [[[a A] [b B]]]
  (or (and (>= a b) (>= B A))
      (and (>= b a) (>= A B))))

(defn overlap? [[[a A] [b B]]]
  (or (<= a b A)
      (<= a B A)
      (<= b a B)))

(defn parse-line [line]
  (->> (s/split line #"[,-]")
       (map #(Integer/parseInt %))
       (partition 2)))

(->> "input4b"
     slurp
     s/split-lines
     (filter (comp  overlap? parse-line))
     count)

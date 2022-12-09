(ns bored.9-test
  (:require
   [clojure.test :refer [deftest is are]]
   [bored.9  :refer [follow spring]]))

(deftest follow-test
  (are [x y X Y] (= [X Y] (spring [x y]))
    0 0 0 0
    1 1 0 0
    -1 -1 0 0
    1 -1 0 0
    2 0 1 0
    2 0 1 0
    2 1 1 1
    2 -1 1 -1
    -1 -2 -1 -1))

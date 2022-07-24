(ns bored.clj-test
  (:require [clojure.test :refer :all]
            [bored.clj :refer :all]))

(deftest a-test
  (is (= -1 (findNb 91716553919377)))
  (is (= 45 (findNb 1071225))))

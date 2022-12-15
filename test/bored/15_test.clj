(ns bored.15-test
  (:require
   [clojure.test :refer [deftest is]]
   [bored.15 :as subject]))

(deftest point->angled-interval-test
  (is (= nil (subject/point->angled-interval [-4 1] [[4 5] 3])))
  (is (= [5 7] (subject/point->angled-interval [-3 1] [[4 5] 3])))
  (is (= [5 6] (subject/point->angled-interval [-2 1] [[4 5] 3])))
  (is (= [4 6] (subject/point->angled-interval [-1 1] [[4 5] 3])))
  (is (= [4 5] (subject/point->angled-interval [0 1] [[4 5] 3])))
  (is (= [3 5] (subject/point->angled-interval [1 1] [[4 5] 3])))
  (is (= nil (subject/point->angled-interval [2 1] [[4 5] 3]))))

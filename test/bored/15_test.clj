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
(deftest point->angled-interval-test2
  (is (= nil (subject/point->angled-interval [-4 1] [[1 3] 2])))
  (is (= [3 4] (subject/point->angled-interval [-3 1] [[1 3] 2])))
  (is (= [3 3] (subject/point->angled-interval [-2 1] [[1 3] 2])))
  (is (= [2 3] (subject/point->angled-interval [-1 1] [[1 3] 2])))
  (is (= nil (subject/point->angled-interval [0 1] [[1 3] 2]))))
(deftest point->angled-interval-test-other-angle
  (is (= nil (subject/point->angled-interval [2 -1] [[1 3] 2])))
  (is (= [2 3] (subject/point->angled-interval [3 -1] [[1 3] 2])))
  (is (= [3 3] (subject/point->angled-interval [4 -1] [[1 3] 2])))
  (is (= [3 4] (subject/point->angled-interval [5 -1] [[1 3] 2])))
  (is (= nil (subject/point->angled-interval [6 -1] [[1 3] 2]))))
(deftest point->angled-interval-test3
  (is (= nil (subject/point->angled-interval [4 1] [[10 10] 4])))
  (is (= [7 10] (subject/point->angled-interval [3 1] [[10 10] 4])))
  (is (= [8 10] (subject/point->angled-interval [2 1] [[10 10] 4])))
  (is (= [8 11] (subject/point->angled-interval [1 1] [[10 10] 4])))
  (is (= [9 11] (subject/point->angled-interval [0 1] [[10 10] 4]))))
(map  #(/ (if (even? %) (dec %) (- % 2)) 2) [2 3 4 5])
(map   [2 3 4 5])

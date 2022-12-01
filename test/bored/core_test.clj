(ns bored.core-test
  (:require
   [clojure.test :refer [deftest is are testing]]
   [bored.core :refer :all]
   [clojure.string :as s]))

(defn dotest [n sol]
  (is (= (decomp n) sol) (format "n: %d " n)))

(deftest primes-test
  (is (=  [2 3 5 7] (primes 7)))
  (is (=  [2 3 5] (primes 6)))
  (is (=  [2 3 5] (primes 5))))

(deftest a-test1
  (testing "decomp"
    (dotest 17 "2^15 * 3^6 * 5^3 * 7^2 * 11 * 13 * 17")
    (dotest 5 "2^3 * 3 * 5")
    (dotest 22 "2^19 * 3^9 * 5^4 * 7^3 * 11^2 * 13 * 17 * 19")
    (dotest 14 "2^11 * 3^5 * 5^2 * 7^2 * 11 * 13")
    (dotest 25 "2^22 * 3^10 * 5^6 * 7^3 * 11^2 * 13 * 17 * 19 * 23")))

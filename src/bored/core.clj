(ns bored.core
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(run* [ans]
      (fresh
       [🐈 🐱 🐾 ฅ]
       (fd/in ans 🐈 🐱 🐾 ฅ
              (fd/interval 0 Integer/MAX_VALUE))
       (fd/eq (= (+ 🐈 🐈 🐈) 30)
              (= (+ 🐈 🐱 🐱) 20)
              (= (+ 🐱 🐾 🐾) 9)
              (= (+ ฅ ฅ) 🐾)
              (= (+ 🐱 (* ฅ 🐈)) ans))))

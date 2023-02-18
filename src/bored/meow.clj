(ns bored.meow
  (:require [clojure.string :as s]
            [clojure.test :as t]))

(def meows (str "(m[- ])?"
                "("
                (s/join "|"
                        ["m+r*[ieao]*[ea]+[ieao]*[łwu]*[eao]*"
                         "m+r+[ieao]+[łwu]+"
                         "m+[oae]?r*[łwu]*"
                         "p+u*rr+"
                         "m*r+((p|a+)?|r)" ; mrrrp mraa
                         ])
                ")"))
(def nyas "(n[- ])?n+[yi]+a+")

(def single-meowgex (str "(" meows "|" nyas ")"))
(def multi-meowgex (str single-meowgex "([^a-zA-Z0-9]+" single-meowgex ")*"))
(def meowgex (str "^ *" multi-meowgex "[?!~ ]*$"))

(def all-words (->> "/etc/dictionaries-common/words"
                    slurp
                    s/split-lines
                    (map s/lower-case)
                    sort
                    dedupe))

(def must-meows ["meow" "mraow" "mrrrp" "mriew" "mriał" "mraaa"])
meowgex
(t/deftest meows-match
  (doseq [meow must-meows]
    (t/is (= meow (-> single-meowgex re-pattern (re-matches meow) first)))))
(filter #(some? (re-matches (re-pattern meowgex) %)) all-words)

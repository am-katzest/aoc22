(ns bored.meow
  (:require [clojure.string :as s]
            [clojure.test :as t]))

(defn any-order [a b]
  (str "(" a b "|" b a ")"))
(defn at-least-one [main single]
  (str "("
       main "+" single main "*"
       "|"
       main "*" single main "+"
       ")"))

(def meows (str "(m[- ])?"
                "("
                (s/join "|"
                        ["m+r*[ieao]*[ea]+[ieao]*[łwu]*[eao]*"
                         "m+r+[ieao]+[łwu]+"
                         "m+[oae]?r*[łwu]*"
                         "p+u*rr+"
                         "m[nmwreaouwi]+[włu]" ; this one should get most
                         (str "r+" (at-least-one "[nmwreaouwi]" "[rea]") "[iawłu]+")
                         "m*r+((p|a+)?|r)" ; mrrrp mraa
                         ])
                ")"))
(def nyas "(n[- ])?n+[mnyi]+a+")

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
(remove #(some? (re-matches (re-pattern meowgex) %))
        ["more"
         "meorew"
         "meoerwew"
         "rmoewoermoww"
         "rroreeoww"
         "rmroeww"
         "mreroew"
         "mroew"])
meowgex

(ns bored.uwu
  (:require [clojure.xml :as xml]
            [clojure.string :as  s]))

(defn read-that-thing [x]
  (println x)
  (->> x
       slurp
       (#(s/replace % #"&#\d+;" ""))
       (#(s/replace-first % #"^[^\n]+\n" ""))
       (#(str "<UWU>" % "</UWU>"))
       .getBytes
       java.io.ByteArrayInputStream.
       xml/parse
       :content))

(->> (range 0 22)
     (map #(format "data/reut2-0%02d.sgm" %))
     (map read-that-thing)
     (apply concat)
     shuffle
     (#(xml/emit {:tag :DATASET
                  :attrs {:UWUŚNY true}
                  :content (vec %)}))
     with-out-str
     (spit "fixed"))

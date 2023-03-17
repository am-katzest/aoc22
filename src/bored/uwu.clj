(ns bored.uwu
  (:require [clojure.xml :as xml]
            [clojure.string :as  s]))

(defn read-that-thing [x]
  (->> x
       (format "data/reut2-0%02d.sgm")
       slurp
       (#(s/replace % #"&#\d+;" ""))
       (#(s/replace-first % #"^[^\n]+\n" ""))
       (#(str "<UWU>" % "</UWU>"))
       .getBytes
       java.io.ByteArrayInputStream.
       xml/parse
       :content))

(->> (range 0 22)
     (pmap read-that-thing)
     (reduce concat)
     shuffle
     (partition-all 1000)
     (pmap  #(with-out-str (mapv xml/emit-element %)))
     (s/join "\n")
     (spit "fixed")
     time)

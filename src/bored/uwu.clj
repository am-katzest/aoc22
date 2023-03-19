(ns bored.uwu
  (:require [clojure.xml :as xml]
            [clojure.string :as  s]
            [clojure.data.json :as json]))

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

(defn strip [x] (apply concat (map :content x)))
(def unroll)
(defn unex [t x]
  (condp  = t
    :DATE (first x)
    :TITLE (first x)
    :BODY (first x)
    :DATELINE (first x)
    :TEXT (unroll x)
    (strip x)))

(defn unroll [x]
  (into {} (map (fn [{:keys [tag content]}] [tag (unex tag content)]) x)))

(defn lower-keys [x]
  (into {} (map (fn [[k v]] [(and k (-> k name s/lower-case keyword)) v]) x)))

(defn parse [x]
  (let [x (-> x
              :content
              unroll)]
    (-> x
        (dissoc :UNKNOWN :MKNOTE :TEXT)
        (into (:TEXT x))
        lower-keys)))

(->> (range 0 22)
     (pmap read-that-thing)
     (reduce concat)
     (pmap parse)
     (remove #(% nil))
     shuffle
     json/write-str
     (spit "json")
     time)
(->> (range 0 1)
     (pmap read-that-thing)
     (reduce concat)
     (pmap parse)
     first
     keys)

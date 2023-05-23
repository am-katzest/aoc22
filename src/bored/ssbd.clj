(ns bored.ssbd
  (:require [clojure.string :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def columns [:League
              :Season
              :Stage
              :Player
              :Team
              :GP
              :MIN
              :FGM
              :FGA
              :3PM
              :3PA
              :FTM
              :FTA
              :TOV
              :PF
              :ORB
              :DRB
              :REB
              :AST
              :STL
              :BLK
              :PTS
              :birth_year
              :birth_month
              :birth_date
              :height
              :height_cm
              :weight
              :weight_kg
              :nationality
              :high_school
              :draft_round
              :draft_pick
              :draft_team])

(defn to-int [x] (Integer/parseInt x))
(defn to-float [x] (Double/valueOf x))
(def  posts {:Season #(to-int (first (s/split % #" ")))
             :Player identity
             :GP to-float
             :MIN to-float
             :FGM to-float
             :FGA to-float
             :3PM to-float
             :3PA to-float
             :FTM to-float
             :FTA to-float
             :PF to-float
             :AST to-float
             :STL to-float
             :PTS to-float
             :birth_year to-int
             :height_cm to-int
             :weight_kg to-int})

(defn read-record [r]
  (try (->> r
            (map (fn [l x] [l ((get posts l identity) x)]) columns)
            (into {}))
       (catch Throwable _ nil)))
(defn  process-record [r]
  (try [(:GP r)
        (- (:Season r) (:birth_year r))
        (:height_cm r)
        (:weight_kg r)
        (/ (:MIN r) (:GP r))
        (/ (:PF r) (:GP r))
        (/ (:AST r) (:GP r))
        (/ (:STL r) (:GP r))
        (/ (:PTS r) (:GP r))
        (/ (+ (:FGM r) (:FTM r) (:3PM r))
           (+ (:FGA r) (:FTA r) (:3PA r)))
        (str (:Player r) " " (:Season r))]
       (catch Throwable _ nil)))

(with-open [reader (io/reader "ksr2.csv")
            writer (io/writer "out.csv")]
  (->>
   (csv/read-csv reader)
   (keep read-record)
   (keep process-record)
   (csv/write-csv writer)))

;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Climate survey modelling (i.e., the Six Americas)
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.survey
  (:require [say.genie          :refer :all]
            [say.log            :as log]
            [weka.tweet         :as tw]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp]]
            [incanter.core      :refer [dataset view with-data]]
            [incanter.charts    :refer [bar-chart]])
  (:import  [weka.core.stemmers SnowballStemmer]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const INIT-SURVEY    :sassy)

;;; Six Americas Surveys:
;;; Key-words do not (currently) include:
;;; - personal pronouns
;;; - think/know
;;;
;;; TODO:
;;; - Model Key-Words as #{general alarmed ... dismissive}
;;; - handle bigrams
(defonce Key-Words  {:six36 (set/union
                             #{"change" "mind" "global" "warming" "issues"          ; Table 2"
                               "information" "opinion"}

                             #{"experts" "global" "warming" "really" "happening"    ; Table 3
                               "cause" "harm" "United" "States" "reduce"}

                             #{"global" "warming" "conserving" "energy"}            ; Table 4

                             #{"caused" "human" "activities" "natural"              ; Table 5
                               "changes" "environment" "happening"}

                             #{"scientists" "global" "warming" "happening"
                               "disagreement"}

                             #{"informed"}

                             #{"interested" "disgusted" "helpless" "hopeful"        ; Table 6
                               "sad" "angry" "afraid" "guilty" "depressed"}

                             #{"important" "issue" "global" "warming" "today"       ; Table 7
                               "worried"}

                             #{"global" "warming" "harm" "personally" "family"      ; Table 8a
                               "community" "people" "United" "States"
                               "modern" "industrialized" "countries" "developing"
                               "future" "generations" "plant" "animal" "species"}

                             #{"global" "warming" "start" "harm" "people" "United"  ; Table 8b
                               "States" "now" "years" "never" "world"}

                             #{"personally" "experienced" "effects" "global"        ; Table 9
                               "warming" "record" "snowstorms" "winter" "eastern"
                               "United" "States" "question" "occurring" "local"
                               "area" "snow" "rain" "different" "normal" "warmer"
                               "colder"}

                             #{"economic" "downturn" "country" "hurt" "family"      ; Table 10
                               "economy" "bad" "shape" "US" "afford" "reduce"
                               "global" "warming"}

                             #{"protecting" "environment" "economic" "growth"       ; Table 11
                               "provides" "jobs" "reduces" "conflict"
                               "environmental" "protection" "important" "problems"}

                             #{; BIGRAMS: "turn off" "air conditioning"             ; Table 12 & 13
                               ;          "brush teeth" "wash dishes"
                               ;          "power strips" "surge protectors"
                               ;          "light bulbs" "compact fluorescent"
                               "lights" "electronics" "TVs" "computers" "recycle"
                               "home" "winter" "thermostat" "degrees" "cooler"
                               "summer" "warmer" "air" "conditioning" "re-usable"
                               "beverage" "container" "reduce" "trash" "garbage"
                               "water" "shower" "brush" "teeth" "wash" "dishes"
                               "unplug" "power" "strips" "surge" "protectors"
                               "walk" "bike" "driving" "public" "transportation"
                               "carpool" "light" "bulbs" "energy-efficient"
                               "compact" "fluorescent" "CFLs"}

                             #{; BIGRAMS: "United States"                           ; Table 14
                               "energy-saving" "actions" "months" "reduce"
                               "personal" "contribution" "global" "warming"
                               "United" "States" "people" "modern" "industrialized"
                               "countries" "world"})

                     :sassy #{"think"
                              "global" "warming"
                              "harm"
                              "future" "generations" "people"
                              "important" "issue" "personally"
                              "worried"}})

(defonce Stem-Words     (update-values Key-Words (fn [words]
                                                   (let [sball (tw/make-stemmer)]
                                                     (into #{} (map #(.stem sball %) words))))))

(defonce Stem-Counts    (agent (into {} (map #(vector % {}) (keys Stem-Words)))))


;;; --------------------------------------------------------------------------
(defn in-survey?
  "Returns true if the specified word is a keyword in the indicated survey."
  ([survey word]
  (in-survey? survey word (tw/make-stemmer)))


  ([survey word sball]
  (let [stem (.stem ^SnowballStemmer sball word)
        in?  (contains? (Stem-Words survey) stem)]
    (when in?
      (send Stem-Counts #(update-in % [survey stem] (fnil inc 0))))
    in?)))



;;; --------------------------------------------------------------------------
(defn report-survey
  "Rerports word (stem) coverage for words checked against surveys associated
  with this  namespace."
  ([]
  (run! report-survey (keys Stem-Words)))


  ([survey & opts]
  (await Stem-Counts)
  (let [stems (sort (fn [[_ a] [_ b]]
                      (> a b))
                    (@Stem-Counts survey))]

    (when (some #{:chart} opts)
      (future
       (with-data (dataset [:stem :freq] stems)
         (view (bar-chart :stem :freq)))))

    (run! (fn [[stem freq]]
            (log/fmt-info "~14a: ~4a" stem freq))
          stems))))


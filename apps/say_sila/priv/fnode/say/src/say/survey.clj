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
            [clojure.pprint     :refer [pp]])
  (:import  [weka.core.stemmers SnowballStemmer]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const INIT-SURVEY    :sassy)

;;; Six Americas Surveys:
;;; Keywords do not (currently) include:
;;; - personal pronouns
;;; - think/know
;;;
;;; TODO:
;;; - Model keywords as #{general alarmed ... dismissive}
;;; - handle bigrams
(defonce Keywords   {:six36 (set/union
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

(defonce Stemwords  (update-values Keywords (fn [words]
                                              (let [sball (tw/make-stemmer)]
                                                (into #{} (map #(.stem sball %) words))))))


;;; --------------------------------------------------------------------------
(defn in-survey?
  "Returns true if the specified word is a keyword in the indicated survey."
  ([survey word]
  (in-survey? survey word (tw/make-stemmer)))


  ([survey word sball]
  (contains? (Stemwords survey)
             (.stem ^SnowballStemmer sball word))))


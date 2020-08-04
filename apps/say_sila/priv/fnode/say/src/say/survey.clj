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
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp]])
  (:import  [weka.core.stemmers SnowballStemmer]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const INIT-SURVEY    :sassy)

(defonce Keywords   {:sassy #{"think"
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


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion and sentiment analysis Ontology
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.senti
  (:require [say.genie      	:refer :all]
            [say.ontology   	:refer :all]
            [say.log        	:as log]
	    [clojure.data.csv 	:as csv]
	    [clojure.java.io 	:as io]
            [clojure.pprint 	:as prt :refer [pp pprint]]
            [tawny.repl     	:as repl]               	; <= DEBUG
            [tawny.owl      	:as owl]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/senti.owl#")
(def ^:const ONT-FPATH  "resources/KB/senti.owl")
(def ^:const DATASET    "resources/emo-sa/sentiment-analysis.csv")

;;; --------------------------------------------------------------------------
(defn create
  "Initial function to create the senti ontology.  Expect changes."
  ([] (create DATASET))

  ([fpath]
    (with-open [rdr (io/reader fpath)]
      (second (csv/read-csv rdr)))))
   

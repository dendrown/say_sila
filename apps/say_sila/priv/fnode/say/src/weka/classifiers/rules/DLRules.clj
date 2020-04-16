;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion Mining and Machine Learning for Climate Change communications
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.classifiers.rules.DLRules
  (:require;[say.genie       :refer :all]
           ;[say.log         :as log]
            [weka.core       :as weka]
            [clojure.string  :as str])
  (:import  (weka.classifiers AbstractClassifier)
            (weka.classifiers.rules DLRules)                    ; (this)
            (weka.core Capabilities
                       Capabilities$Capability
                       Instance))
  (:gen-class
    :name       weka.classifiers.rules.DLRules
    :extends    weka.classifiers.AbstractClassifier
    :state      state
    :init       init
    :implements [weka.core.CapabilitiesHandler]

    :exposes-methods {getCapabilities   superGetCapabilities}
))

(set! *warn-on-reflection* true)



;; ---------------------------------------------------------------------------
(defn -init
  "Initializes the classifier's state."
  []
  [[] (atom {})])



;; ---------------------------------------------------------------------------
(defn -getCapabilities
  "Returns the Capabilities of this classifier."
  [^DLRules this]
  (let [caps (doto (.superGetCapabilities this)
                   (.disableAll))]
    (run! #(.enable caps %)
          [Capabilities$Capability/NOMINAL_ATTRIBUTES
           Capabilities$Capability/NUMERIC_ATTRIBUTES
           Capabilities$Capability/STRING_ATTRIBUTES])
    caps))



;; ---------------------------------------------------------------------------
(defn -classifyInstance
  "Classifies the given test instance. The instance has to belong to a dataset
  when it's being classified."
  [^Instance ins]
  0.0)

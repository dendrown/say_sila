;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Weka access to the emotion and sentiment analysis Ontology
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.senti
  (:require [say.genie          :refer :all]
            [say.log            :as log]
            [say.senti          :as senti]
            [weka.core          :as weka]
            [clojure.pprint     :refer [pp pprint]]
            [tawny.owl          :refer [save-ontology]])
  (:import  (weka.classifiers AbstractClassifier)
            (weka.core Capabilities
                       Capabilities$Capability
                       Instance
                       Instances)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(defonce NS (keyize *ns*))


;;; --------------------------------------------------------------------------
(defn ^AbstractClassifier make-classifier
  "Proxies a Weka classifier to handle evaluate tweets using the say-senti
  ontology."
  [& opts]
  (let []
    (proxy [AbstractClassifier]
    []

    ;; -----------------------------------------------------------------------
    (getCapabilities []
      (let [^AbstractClassifier this this
            ^Capabilities       caps (proxy-super getCapabilities)]

        (.disableAll caps)
        (run! #(.enable caps %)
              [Capabilities$Capability/NOMINAL_ATTRIBUTES
               Capabilities$Capability/NUMERIC_ATTRIBUTES
               Capabilities$Capability/STRING_ATTRIBUTES])
        caps))

    ;; -----------------------------------------------------------------------
    (distributionForInstance [^Instance inst]
      ;; FIXME: Call -distributionsForInstances
      (double-array [1. 0.]))

    ;; -----------------------------------------------------------------------
    (distributionsForInstances [^Instances insts]
      (let [rows  (.numInstances insts)
            dists (make-array Double/TYPE rows 2)           ; Assume binary class!
            xmps  (senti/instances->examples insts)
            ont   (senti/populate-ontology NS xmps)]

        ;; Provide some debugging feedback
        (log/debug "Example:" (first xmps))
        (save-ontology ont (str "/tmp/" (name NS) ".owl") :owl)

        ;; Return our predictions
        dists)))))


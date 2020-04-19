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
            [tawny.owl          :refer :all]
            [tawny.reasoner     :as rsn])
  (:import  (weka.classifiers AbstractClassifier)
            (weka.core Capabilities
                       Capabilities$Capability
                       Instance
                       Instances)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const POS "LearnedPositiveText")

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
      (let [rows    (.numInstances insts)
            dists   (make-array Double/TYPE rows 2)             ; Assume binary class!
            xmps    (senti/instances->examples insts)
            ont     (senti/populate-ontology NS xmps)
            rsnr    (senti/reason :hermit ont)                  ; This will run checks!
            ptexts  (rsn/instances ont (owl-class ont POS))     ; Predicted positive texts
            np->01  #(if (contains? ptexts %) 1 0)]             ; Index: neg=0, pos=1
        ;; Provide some debugging feedback
        (log/fmt-debug "Data<~a/~a>: xmp[~a]" (count ptexts) rows (first xmps))
        (save-ontology ont (str "/tmp/" (name NS) ".owl") :owl)

        ;; Check instance IDs against the ontology's positive Texts
        (run! #(let [inst (.get insts %)
                     tid  (senti/label-text inst)]
                 (aset-double dists % (np->01 tid)))
              (range 1 (inc rows)))                             ; Instances 0..N-1

        ;; Return our predictions
        dists)))))


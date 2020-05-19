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
            [say.jvm            :as jvm]
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

(def ^:const SLICE-CNT  5)                  ; Small batches are generally faster


;;; --------------------------------------------------------------------------
(defn- distribution-for-slice
  "Evaluates a subset of instances with respect to the say-senti ontology."
  [^Instances insts
              dists
              solns
   ^Long      i0
   ^Long      cnt]
  (log/debug "Memory:" (jvm/memory-used :MB) "MB")
  (let [xmps    (senti/instances->examples (Instances. insts i0 cnt))
        ont     (senti/populate-ontology :eval xmps solns)
        rsnr    (senti/reason :hermit ont :no-log)              ; This will run checks!
        learned (owl-class ont senti/LEARNED-POS)               ; DL-Learner equivalent soln
        ptexts  (rsn/instances ont learned)                     ; Predicted positive texts
        np->01  #(if (contains? ptexts(individual ont %)) 1 0)] ; Index: neg=0, pos=1

    (comment ;; Debugging feedback:
      (log/debug "Examples:" (count xmps))
      (log/debug "Learned:" (rsn/isubclasses ont learned))
      (log/fmt-debug "Data<~a/~a>: xmp[~a ...]" (count ptexts) cnt (first ptexts)))

    ;; Check instance IDs against the ontology's positive Texts
    (run! #(let [ndx  (long (+ i0 %))
                 inst (.get insts ndx)
                 tid  (senti/label-text inst)]
             (aset-double dists ndx (np->01 tid) 1.))           ; Class slot is 100%
          (range cnt))                                          ; Slice offset 0..N-1

    (save-ontology ont (str "/tmp/" (.relationName insts) "." i0 "+" cnt ".owl") :owl)))



;;; --------------------------------------------------------------------------
(defn ^AbstractClassifier make-classifier
  "Proxies a Weka classifier to handle evaluate tweets using the say-senti
  ontology."
  [& opts]
  (proxy [AbstractClassifier] []

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
      ;; We just run a batch of one
      (let [this  ^AbstractClassifier this
            insts (doto (senti/rebase-data)
                        (senti/add-instance inst))
            dists (.distributionsForInstances this insts)]
        (aget dists 0)))

    ;; -----------------------------------------------------------------------
    (distributionsForInstances [^Instances insts]
      (binding [rsn/*reasoner-progress-monitor* (atom rsn/reasoner-progress-monitor-silent)]
        (let [rows  (.numInstances insts)
              dists (make-array Double/TYPE rows 2)         ; Assume binary class!
              solns (senti/read-solutions)                  ; Using default [TODO]
              cut   #(let [togo (- rows %)]                 ; Cut into slices:
                       ;; Is there something to cut?
                       (when (< % rows)
                         ;; The last slice may be smaller.
                         (if (> togo SLICE-CNT) SLICE-CNT togo)))]

        ;; Evaluate in slice by slice
        (loop [i 0]
          (when-let [cnt (cut i)]
            (distribution-for-slice insts dists solns i cnt)
            (recur (+ i SLICE-CNT))))

          ;; Return our predictions
          dists)))

    ;; -----------------------------------------------------------------------
    (implementsMoreEfficientBatchPrediction []
       true)))


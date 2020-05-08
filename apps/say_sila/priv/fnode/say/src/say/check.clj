;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Checks, verification, and data analysis for Say-Sila (sub)projects
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.check
  (:require [say.genie          :refer :all]
            [say.log            :as log]
           ;[say.influence]                                 ; <= article: Big Players
            [say.senti          :as senti]
            [weka.core          :as weka]
            [weka.senti         :as wsenti])
  (:import  (weka.classifiers Evaluation)
            (weka.core Instances)))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
(defn run-senti
  "Handles test & verification for say-senti ontology-based learning."
  []
  (let [testers (senti/which-arff :test)
                ;"resources/emo-sa/sentiment-analysis.Sentiment140.r24816.train.000.arff"
        target  (senti/which-target)
        insts   (weka/load-arff testers target)
        classer (wsenti/make-classifier)
        audit   (Evaluation. (Instances. insts 0))]

    (log/fmt-info "Testing ~a: arff[~a] cnt[~a]" target
                                                 testers
                                                 (.numInstances insts))
    (.evaluateModel audit classer insts NO-OBJS)
    (log/info "Summary:\n" (.toSummaryString audit))
    (log/info "Class Details\n" (.toClassDetailsString audit))
    (log/info "Confusion Matrix\n" (.toMatrixString audit))
    classer))


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion Mining and Machine Learning for Climate Change communication
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.weka-filter
  (:require [say.genie       :refer :all]
            [say.log         :as log]
            [clojure.java.io :as io]
            [clojure.string  :as str])
  (:import  [affective.core ArffLexiconEvaluator]
            [weka.core Attribute
                       DenseInstance
                       Instance
                       Instances]
            [weka.filters Filter
                          SimpleBatchFilter]))

(set! *warn-on-reflection* true)


;; ---------------------------------------------------------------------------
(defn make-gender-name-filter
  "
  Returns a Weka batch Filter to count female/male names in Instance Attributes.
  "
  []
  (proxy [SimpleBatchFilter] []

    ;; -----------------------------------------------------------------------
    (globalInfo []
      "A Filter to count female/male names in Instance Attributes.")
      

    ;; -----------------------------------------------------------------------
    (determineOutputFormat [^Instances iinsts]
      (let [acnt (.numAttributes iinsts)
            attr (Attribute. "gender-name")]
        ;;
        ;; TODO: We'll need a better attr naming scheme
        (doto (Instances. iinsts 0)
              (.insertAttributeAt attr acnt))))
    

    ;; -----------------------------------------------------------------------
    (process [^Instances iinsts]
      (let [oinsts  (Instances. ^Instances (.determineOutputFormat this iinsts) 0)
            ocnt    (.numAttributes oinsts)
            icnt    (.numAttributes iinsts)
            fndx    (dec ocnt)]
        (doseq [^Instance iinst (seq iinsts)]
          (let [oattrs (double-array ocnt)]
            (dotimes [i icnt]
              (aset oattrs i (.value iinst i)))
            (aset oattrs fndx 0.00)
            (.add oinsts (DenseInstance. 1.0 oattrs))))
        oinsts))))

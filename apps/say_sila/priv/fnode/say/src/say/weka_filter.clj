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
                       Capabilities
                       Capabilities$Capability
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
  (letfn [(^Instances shape-oinsts [^Instances iinsts]
            (let [acnt (.numAttributes iinsts)
                  attr (Attribute. "gender-name")]
             ;;
             ;; TODO: We'll need a better attr naming scheme
             (doto (Instances. iinsts 0)
                   (.insertAttributeAt attr acnt))))]


    (proxy [SimpleBatchFilter] []

      ;; ---------------------------------------------------------------------
      (globalInfo []
        "A Filter to count female/male names in Instance Attributes.")


      (determineOutputFormat [iinsts]
        (shape-oinsts iinsts))
    

      (getCapabilities []
        (let [^SimpleBatchFilter this this
              ^Capabilities      caps (proxy-super getCapabilities)]
          (doto caps
                (.enableAllAttributes)
                (.enableAllClasses)
                (.enable Capabilities$Capability/NO_CLASS))))


      (process [^Instances iinsts]
        (let [oinsts  (Instances. ^Instances (shape-oinsts iinsts) 0)
              ocnt    (.numAttributes oinsts)
              icnt    (.numAttributes iinsts)
              fndx    (dec ocnt)]
          (doseq [^Instance iinst (seq iinsts)]
            (let [oattrs (double-array ocnt)]
              (dotimes [i icnt]
                (aset oattrs i (.value iinst i)))
              (aset oattrs fndx 0.00)
              (.add oinsts (DenseInstance. 1.0 oattrs))))
          oinsts)))))


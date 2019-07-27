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
                       Instances
                       SingleIndex]
           ;[weka.filters Filter
           ;              SimpleBatchFilter]
            [weka.filters.unsupervised.attribute TweetToFeatureVector]))

(set! *warn-on-reflection* true)

(def ^:const GENDER-GOLD "/srv/say_sila/weka/gender.pan2014.1-2.arff")      ; Subset!!
(def ^:const NAMES       (read-string (slurp "resources/gender/names.edn")))


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


    (proxy [TweetToFeatureVector] []

      ;; ---------------------------------------------------------------------
      (globalInfo []
        "A Filter to count female/male names in Instance Attributes.")


      (determineOutputFormat [iinsts]
        (shape-oinsts iinsts))
    

      (process [^Instances iinsts]
        (let [^TweetToFeatureVector this this
              oinsts  (Instances. ^Instances (shape-oinsts iinsts) 0)
              ocnt    (.numAttributes oinsts)
              icnt    (.numAttributes iinsts)
              flt-ndx (dec ocnt)
              txt-ndx (proxy-super getTextIndex)]   ; FIXME: string value
          ;; FIXME: No access to parent's m_textIndex via proxy
          ;;        I was trying to avoid gen-class :(
          ;;(.setUpper ^SingleIndex txt-ndx (dec icnt))
          (doseq [^Instance iinst (seq iinsts)]
            (let [oattrs (double-array ocnt)]
              (dotimes [i icnt]
                (aset oattrs i (.value iinst i)))
              (aset oattrs flt-ndx 0.00)
              (.add oinsts (DenseInstance. 1.0 oattrs))))
          oinsts)))))


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
(ns weka.filters.unsupervised.attribute.TweetToGenderFeatures
  (:require [say.genie       :refer :all]
            [say.log         :as log]
            [clojure.string  :as str])
  (:import  [affective.core ArffLexiconEvaluator]
            [weka.core Attribute
                       Capabilities
                       Capabilities$Capability
                       DenseInstance
                       Instance
                       Instances
                       SingleIndex]
            [weka.filters.unsupervised.attribute TweetToGenderFeatures  ; (this)
                                                 TweetToFeatureVector])
  (:gen-class
    :name       weka.filters.unsupervised.attribute.TweetToGenderFeatures
    :extends    weka.filters.unsupervised.attribute.TweetToFeatureVector
    :exposes    {m_textIndex {:get getTextIndex :set setTextIndex}}))

(set! *warn-on-reflection* true)

(def ^:const -GENDER-GOLD "/srv/say_sila/weka/gender.pan2014.1-2.arff")      ; Subset!!
(def ^:const -NAMES       (read-string (slurp "resources/gender/names.edn")))


;; ---------------------------------------------------------------------------
(defn -globalInfo
  "
  Returns a string description of this filter
  "
  [this]
  "A Filter to count female/male names in Instance Attributes.")


;; ---------------------------------------------------------------------------
(defn -determineOutputFormat
  "
  Returns a empty Instances object, which defines the filter's output after
  processing a set of input Instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             iinsts]
  (let [acnt (.numAttributes iinsts)
        attr (Attribute. "gender-name")
        tndx (.getTextIndex this)]

    ;; Make sure the user stays in bounds for the tweet text attribute
    (.setUpper ^SingleIndex tndx (dec acnt))

    ;; TODO: We'll need a better attr naming scheme
    (doto (Instances. iinsts 0)
          (.insertAttributeAt attr acnt))))


;; ---------------------------------------------------------------------------
(defn -process
  "
  Run the filter on the input instances and return the output instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             iinsts]
  (let [oinsts  (Instances. ^Instances (-determineOutputFormat this iinsts) 0)
        ocnt    (.numAttributes oinsts)
        icnt    (.numAttributes iinsts)
        iattr   (.getIndex ^SingleIndex (.getTextIndex this))
        ondx    (dec ocnt)]
    (doseq [^Instance iinst (seq iinsts)]
      (let [ovals (double-array ocnt)]
        (dotimes [i icnt]
          (aset ovals i (.value iinst i)))
        (aset ovals ondx 0.00)
        (.add oinsts (DenseInstance. 1.0 ovals))))
          oinsts))


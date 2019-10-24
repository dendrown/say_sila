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
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.dolce          :as dul]
            [say.log            :as log]
            [say.cmu-pos        :as pos]
            [weka.core          :as weka]
            [clojure.core.match :refer [match]]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.string     :as str]
            [clojure.pprint     :as prt :refer [pp pprint]]
            [tawny.english      :as dl]
            [tawny.repl         :as repl]                       ; <= DEBUG
            [tawny.owl          :refer :all])
  (:import  [weka.core DenseInstance
                       Instance
                       Instances]
            [weka.filters.unsupervised.attribute TweetNLPPOSTagger
                                                 TweetToSentiStrengthFeatureVector]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-senti.owl#")
(def ^:const ONT-FPATH  "resources/KB/say-senti.owl")
(def ^:const DATASET    "resources/emo-sa/sentiment-analysis.csv")
(def ^:const ARFFs      {:base          "resources/emo-sa/sentiment-analysis.arff"
                         :Sentiment140  "resources/emo-sa/sentiment-analysis.Sentiment140.arff"
                         :Kaggle        "resources/emo-sa/sentiment-analysis.Kaggle.arff"})
(def ^:const COL-ID     0)
(def ^:const COL-TEXT   1)

(defonce Examples-pos (atom {}))        ; FIXME: We don't really want this


;;; --------------------------------------------------------------------------
(defontology say-senti
  :iri     ONT-IRI
  :prefix  "senti"
  :comment "Ontology for training sentiment models.")

(doseq [imp [dul/dul
             pos/cmu-pos]]
  (owl-import imp))


(defcopy dul/associatedWith)
(defcopy dul/follows)
(defcopy dul/directlyFollows)

(defcopy dul/precedes)
(defcopy dul/directlyPrecedes)

(defcopy dul/hasPart)
(defcopy dul/hasComponent)

(as-subclasses dul/InformationObject
  :disjoint
  (defclass Text
    :label   "Text"
    :comment "An Information Object consisting of text.")

  (defclass Term
    ;TODO:  Consider splitting off: numeral, emoticon, hashtag, @mention
    :label   "Term"
    :comment "An Information Object representing a syntactic unit of meaning, such as a word.")

  (defclass Punctuation
    :label   "Punctuation"
    :comment (str "An Information Object representing a grammatical symbol to organize and"
                  "aid the understanding of written text.")))

(defcopy pos/Token)
(refine Token :equivalent (dl/or Term Punctuation))

;; FIXME: I think we can drop hasIdentifier because the ID names the indivdual
(defdproperty hasIdentifier
  :domain dul/InformationEntity
  :range  :XSD_NON_NEGATIVE_INTEGER
  :characteristic :functional)


;;; --------------------------------------------------------------------------
(defn add-text
  "Adds a text individual to the ontology given a numeric identifier, n, and
  a list of part-of-speech tags for term in the text."
  [n pos-tags]
  (let [id (str "t" n)]

    ;; Add an entity representing the text itself
    (individual id
      :type Text
      :fact (is hasIdentifier id))

    ;; And entities for each of the terms, linking them together and to the text
    (reduce
      (fn [info tag]
        (let [cnt  (:cnt info)
              tid  (str id "-" cnt)
              curr (individual tid
                     :type  Token
                     :label (str tid " (" tag ")")
                     :fact  (is hasIdentifier tag))]

          ;; Set POS Quality
          (when-let [pos (pos/lookup# tag)]
            (refine curr :fact (is pos/isPartOfSpeech pos)))

          ;; Link tokens to each other
          (when-let [prev (:prev info)]
            (refine prev :fact (is dul/directlyPrecedes curr))
            (refine curr :fact (is dul/directlyFollows  prev)))

          ;; Continue the reduction
          {:cnt (inc cnt), :prev curr}))

      {:cnt 0}
      pos-tags)))



;;; --------------------------------------------------------------------------
(defn populate
  "Populates the senti ontology using examples from the ARFFs"
  []
  ;; FIXME: Pass in the values from the ARFF
  (doseq [[id poss] @Examples-pos]
    (add-text id poss)))



;;; --------------------------------------------------------------------------
(defn create-pos
  "Create examples based on part-of-speech tokens"
  ([] (create-pos :Sentiment140))


  ([dset]
  (let [arff  (ARFFs dset)
        insts (weka/load-arff arff)]
    (reset! Examples-pos
            (reduce (fn [acc ^Instance inst]
                      (let [id    (long (.value inst COL-ID))
                            toks  (str/split (.stringValue inst COL-TEXT) #" ")
                            poss  (map #(first (str/split % #"_" 2)) toks)]
                       (assoc acc id poss)))
                    {}
                    (enumeration-seq (.enumerateInstances insts)))))))



;;; --------------------------------------------------------------------------
(defn create-arffs
  "Initial function to create the senti ontology.  Expect changes."
  ([] (create-arffs DATASET))

  ([fpath]
  (let [dsets   (atom {})
        base    (weka/load-arff (:base ARFFs))
        rname   (.relationName base)
        acnt    (.numAttributes base)

        tamer   (doto (TweetToSentiStrengthFeatureVector.)
                      (.setTextIndex "2")                       ; 1-based index
                      (.setStandarizeUrlsUsers true)            ; anonymize
                      (.setReduceRepeatedLetters true))         ; loooove => loove
        tagger  (doto (TweetNLPPOSTagger.)
                      (.setTextIndex "2"))                      ; 1-based index

        label   #(match % "0" "neg"
                          "1" "pos")

        xform   (fn [iinsts]
                  (reduce
                    #(weka/filter-instances %1 %2)
                    iinsts
                   [tamer tagger]))

        dset+   (fn [src]
                  ; Make new dataset
                  (let [insts (doto (Instances. base 0)
                                    (.setRelationName (str rname src)))]
                    (log/info "Adding dataset" src)
                    (swap! dsets assoc src insts)
                    insts))

        data+   (fn [[id pn src raw]]
                  ;; Add data Instance; remember, the Weka objects are mutable
                  (let [text  (str/trim raw)
                        insts (if-let [ds (get @dsets src)] ds (dset+ src))
                        inst  (doto (DenseInstance. acnt)
                                    (.setDataset insts)
                                    (.setValue 0 (Float/parseFloat id))
                                    (.setValue 1 ^String text)
                                    (.setValue 2 (Float/parseFloat pn)))]     ; 0.0=neg, 1.0=pos
                      (log/fmt-debug "~a<~a> [~a] ~a" src id (label pn) text)
                      (.add ^Instances insts inst)))]

    ;; Process the CSV, separating the sentiment sources
    (with-open [rdr (io/reader fpath)]
      (let [[_ & dlines] (csv/read-csv rdr)]
         ;; FIXME: use a subset until we get everything squared away
         (doseq [line (take 1000 dlines)]
           (data+ line))))

    ;; Save the ARFFs to disk and return the result info in a map
    (reduce (fn [acc [dset iinsts]]
              (assoc acc (keyword dset)
                         (weka/save-file fpath dset (xform iinsts) :arff)))
            {}
            @dsets))))

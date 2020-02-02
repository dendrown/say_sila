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
;;;; @copyright 2019-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.senti
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.dolce          :as dul]
            [say.log            :as log]
            [say.cmu-pos        :as pos]
            [weka.core          :as weka]
            [weka.tweet         :as tw]
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
(def ^:const TWEET-TAG  "t")            ; Tweet individual have this tag plus the ID, e.g., "t42"
(def ^:const NUM-EXAMPLES 400)          ; FIXME: use a subset until we get everything squared away


;;; --------------------------------------------------------------------------
;;; TODO: we have a number of decisions that are not yet final...
(def ^:const IMPORT?    false)
(def ^:const POS-NEG?   false)


(defonce Examples   (atom {}))        ; FIXME: We don't really want this


;;; --------------------------------------------------------------------------
(defontology say-senti
  :iri     ONT-IRI
  :prefix  "senti"
  :comment "Ontology for training sentiment models.")

;; Are we importing the full DUL foundational ontology?
(if IMPORT?
  (owl-import dul/dul)
  (do
    ;; We want a minimal ontology.  Recreate just the parts of DUL that we need.
    (refine dul/InformationEntity   :super dul/Entity)
    (refine dul/InformationObject   :super dul/InformationEntity)

    (refine dul/Objekt              :super dul/Entity)
    (refine dul/SocialObject        :super dul/Objekt)
    (refine dul/Concept             :super dul/SocialObject)

    (defcopy dul/associatedWith)
    (as-inverse
      (defcopy dul/precedes)
      (defcopy dul/follows))

    (doseq [op [dul/expresses               ; Do we want isExpressedBy ?
                dul/precedes
                dul/follows]]
      (refine op :super dul/associatedWith
                 :characteristic :transitive))

    (as-inverse
      (defcopy dul/directlyPrecedes)
      (defcopy dul/directlyFollows))
    (refine dul/directlyPrecedes :super dul/precedes)
    (refine dul/directlyFollows  :super dul/follows)


    (defcopy dul/hasPart)
    (refine dul/hasPart :super dul/associatedWith :characteristic :transitive)

    (defcopy dul/hasComponent)
    (refine dul/hasComponent :super dul/hasPart)))



;;; --------------------------------------------------------------------------
(owl-import pos/cmu-pos)
(as-subclasses dul/InformationObject
  :disjoint
  (defclass Text
    :label   "Text"
    :comment "An Information Object consisting of text.")

  ; TODO: Differentiate between Punctuation as an Information Object and a "Part of Speech" Quality
  (defclass Punctuation
    :label   "Punctuation"
    :comment (str "An Information Object representing a grammatical symbol to organize and"
                  "aid the understanding of written text."))

  (defclass Term
    ;TODO:  Consider splitting off: numeral, emoticon, hashtag, @mention
    :label   "Term"
    :comment "An Information Object representing a syntactic unit of meaning, such as a word."))

(defcopy pos/Token)
(refine Token :equivalent (dl/or Term Punctuation))

;; DL-Learner isn't handling Pos/Neg Text subclasses well
(when POS-NEG?
  (as-subclasses Text
    :disjoint
    (defclass NegativeText
      :label "Negative Text"
      :comment "A Text which expresses sentiment of a negative polarity")

    (defclass PositiveText
      :label "Positive Text"
      :comment "A Text which expresses sentiment of a positive polarity.")))


;;; --------------------------------------------------------------------------
;;; Sentiment Composition Rules
;;; \ref{bing2015}
;;; --------------------------------------------------------------------------
(defclass SentimentCompositionRule
  :super   dul/Concept
  :label   "Sentiment Composition Rule"
  :comment (str "An abstraction describing a Text or portion of a Text according to its
                 positive or negative contribution to the polarity of that Text."))

(defmacro defscr
  "Adds a Sentiment Composition Rule (component) subclass to the say-senti ontology"
  [tag descr]
  `(do (defclass ~tag
         :super   SentimentCompositionRule
         :label   (str "Sentiment Composition Rule - " (name '~tag))
         :comment ~descr)
       (defpun ~tag)))

(defscr P   "An atomic, nonterminal sentiment composition expressing positive sentiment.")
(defscr N   "An atomic, nonterminal sentiment composition expressing negative sentiment.")


(defclass SentimentPolarity
  :super   dul/Quality
  :label   "Sentiment Polarity"
  :comment "A Quality describing an Information Object's expression as positive, negative, or neutral.")

(as-subclasses SentimentPolarity
  :disjoint
  (defclass PositiveSentimentPolarity
    :label   "Positive Sentiment Polarity"
    :comment "A Sentiment Polarity expressing positive sentiment")
  (defclass NegativeSentimentPolarity
    :label   "Negative Sentiment Polarity"
    :comment "A Sentiment Polarity expressing negative sentiment"))
(defpun PositiveSentimentPolarity)
(defpun NegativeSentimentPolarity)

(defoproperty hasPolarity
  :super    dul/hasQuality
  :label    "has Polarity"
  :domain   dul/InformationObject
  :range    SentimentPolarity)


;;; --------------------------------------------------------------------------
(defprotocol Polarizer
  "Determines negative|positive polarity for various datatypes."
  (polarize [x] "Return the sentiment polarity as :positive or :negative."))

(extend-protocol Polarizer
  Object
  (polarize [x]
    (if (<= x 0.0)
        :negative
        :positive))

  Instance
  (polarize [inst]
    (polarize (.classValue inst))))


;;; --------------------------------------------------------------------------
(defn add-text
  "Adds a text individual to the ontology given a numeric identifier, n, and
  a list of part-of-speech tags for term in the text."
  [n {:keys [polarity
             pos-tags
             tokens-pn]}]
  ;; The code will assume there's at least one token, so make sure!
  (when (seq pos-tags)
    (let [id      (str TWEET-TAG n)
          express #(refine %1 :fact (is dul/expresses (individual %2)))]

      ;; Add an entity representing the text itself.  Note that we'll be creating
      ;; the referenced token "tN-1" in the reduce expression below.
      (individual id
        :type (if POS-NEG?
                  (case polarity :negative NegativeText
                                 :positive PositiveText)
                  Text)
        :fact (is dul/hasComponent (individual (str id "-1"))))

      ;; And entities for each of the terms, linking them together and to the text
      (reduce
        (fn [info [tag pn]]
          (let [cnt  (:cnt info)
                tid  (str id "-" cnt)
                curr (individual tid
                       :type  Token
                       :label (str tid " (" tag ")"))]

            ;; Set POS Quality
            (if-let [pos (pos/lookup# tag)]
              (refine curr :fact (is pos/isPartOfSpeech pos))
              (log/warn "No POS tag:" tag))

            ;; Link tokens to each other
            (when-let [prev (:prev info)]
              (refine curr :fact (is dul/directlyFollows prev)))

            ;; Express sentiment composition rules
            (case pn
              :p (express curr "P")
              :n (express curr "N")
              :- :ok)

            ;; Continue the reduction
            {:cnt (inc cnt), :prev curr}))

        {:cnt 1}
        (zip pos-tags tokens-pn)))))



;;; --------------------------------------------------------------------------
(defn populate
  "Populates the senti ontology using examples from the ARFFs"
  []
  ;; FIXME: Pass in the values from the ARFF
  (doseq [[id example] @Examples]
    (add-text id example)))



;;; --------------------------------------------------------------------------
(defn create-examples
  "Create examples based on part-of-speech tokens.

  Example: [10 {:pos-tags («!» «,» «O» «V» «R» «O» «D» «N» «E»),
                :tokens-pn (:-  :-  :-  :p  :-  :-  :-  :-  :-),
                 :polarity :positive}]"
  ([] (create-examples :Sentiment140))


  ([dset]
  (let [arff  (ARFFs dset)
        insts (weka/load-arff arff "sentiment")
        lex   (tw/make-pn-lexicon :bing-liu)
        ->pn  #(case (.retrieveValue lex %)
                 "positive"  :p
                 "negative"  :n
                 "not_found" :-)]
    (reset! Examples
            (reduce (fn [acc ^Instance inst]
                      (let [id    (long (.value inst COL-ID))
                            pairs (map #(str/split % #"_" 2)
                                        (str/split (.stringValue inst COL-TEXT) #" "))
                            poss  (map first pairs)
                            poles (map #(->pn (second %)) pairs)]
                       (assoc acc id {:pos-tags  poss
                                      :tokens-pn poles
                                      :polarity (polarize inst)})))
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
                      (.setToLowerCase true)
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
         (doseq [line (take NUM-EXAMPLES dlines)]
           (data+ line))))

    ;; Save the ARFFs to disk and return the result info in a map
    (reduce (fn [acc [dset iinsts]]
              (assoc acc (keyword dset)
                         (weka/save-file fpath dset (xform iinsts) :arff)))
            {}
            @dsets))))



;;; --------------------------------------------------------------------------
(defn pn-examples
  "Prints the IDs of the tweets with positive polarities and those with
  negative polarities.  The caller may optionally specify a prefix for easy
  insertion into a DL-Learner configuration file."
  ([]
  (pn-examples nil))

  ([prefix]
  (pn-examples prefix 8))

  ([prefix n]
  (let [delims  (conj (repeat \,) \space)
        tag     (str prefix (when prefix ":") TWEET-TAG)
        tagger  #(str tag %)
        liner   #(apply print "\n    " %1 (interpose \, (domap pr-str %2)))
        ids     (reduce (fn [[p n] [id info]]
                          (case (:polarity info)
                            :positive [(conj p id) n]
                            :negative [p (conj n id)]))
                        (repeat 2 (sorted-set))             ; Collect IDs for pos/neg examples
                        @Examples)
        xmps    (map #(map tagger %) ids)]                  ; Prefix % tag pos/neg IDs

    ;; Report our P/N examples
    (doseq [[klass xs] (zip ["POSITIVE" "NEGATIVE"] xmps)]  ; Title X examples
      (print klass ": {")
      (domap liner delims (partition-all n xs))
      (println "\n}")))))


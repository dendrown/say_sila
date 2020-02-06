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
            [say.dllearner      :as dll]
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
(def ^:const ONT-PREFIX "senti")
(def ^:const DATASET    "resources/emo-sa/sentiment-analysis.csv")
(def ^:const ARFFs      {:base          "resources/emo-sa/sentiment-analysis.arff"
                         :Sentiment140  "resources/emo-sa/sentiment-analysis.Sentiment140.arff"
                         :Kaggle        "resources/emo-sa/sentiment-analysis.Kaggle.arff"})
(def ^:const COL-ID     0)
(def ^:const COL-TEXT   1)

(def ^:const TWEET-TAG      "t")        ; Tweet individual have this tag plus the ID, e.g., "t42"
(def ^:const NUM-EXAMPLES   4000)       ; FIXME: use a subset until we get everything squared away

(def ^:const EXPR-DECREASE  ["alleviate" "attenuate" "block" "cancel" "cease" "combat"  ; "come down"
                             "crackdown"        ; "crack down"
                             "cut"              ; "cut back" "cut down" "cut off" "cut out"
                                                ; "die off" "die out"
                             "decrease" "deduct" "diminish" "disappear" "discontinue"
                             "discount" "downgrade" "drop" "dwindle" "eliminate" "fade"
                             "fall" "filter"    ; "get around" "get off" "get over" "go away" "go down"
                             "halt"             ; "have gone"
                            ])


(def ^:const EXPRESSIONS    {"DECREASE-N"   #{"alleviate" "avoid" "handle" "lessen" "mitigate" "relieve"
                                              "resolve" "soothe" "subside" "waive"}

                             "DECREASE-P"   #{"lack" "lose" "omit" "miss"}
                             "INCREASE-N"   #{"burst" "climb" "escalate" "intensify"
                                             ; 2-grams: "go up" "grow" "mark up" "pile up"
                                             }
                             "INCREASE-P"   #{"elevate" "enlarge" "expand" "extend" "increase" "progress"
                                              "raise" "return" "rise" "soar" "surge"
                                             ; 2-grams: "be up" "build up" "come back"
                                             }})



;;; --------------------------------------------------------------------------
;;; TODO: we have a number of decisions that are not yet final...
(def ^:const IMPORT?    false)
(def ^:const POS-NEG?   false)


(defonce Examples   (atom {}))        ; FIXME: We don't really want this


;;; --------------------------------------------------------------------------
(defontology say-senti
  :iri     ONT-IRI
  :prefix  ONT-PREFIX
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

(defscr P           "An atomic, nonterminal sentiment composition expressing positive sentiment.")
(defscr N           "An atomic, nonterminal sentiment composition expressing negative sentiment.")
(defscr DECREASE-N  "Expressions which decrease NPI and NE terms.")
(defscr DECREASE-P  "Expressions which decrease PPI and PO terms.")


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
             rules]}]
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
        (fn [info [tag rules]]
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
            (doseq [rule rules]
              (when-not (contains? #{"P" "N"} rule)
                (log/debug "Tweet token" tid "expresses" rule))
              (express curr rule))

            ;; Continue the reduction
            {:cnt (inc cnt), :prev curr}))

        {:cnt 1}
        (zip pos-tags rules)))))



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

  Example:  #10   hmmmm.... i wonder how she my number @-)

            [10 {:pos-tags  («!» «,» «O»   «V»  «R» «O» «D» «N» «E»),
                 :tokens-pn (#{} #{} #{} #{«P»} #{} #{} #{} #{} #{}),
                 :polarity :positive}]"
  ([] (create-examples :Sentiment140))


  ([dset]
  (let [arff  (ARFFs dset)
        insts (weka/load-arff arff "sentiment")
        sball (tw/make-stemmer)
        stem  #(.stem sball %)
        exprs (update-values EXPRESSIONS
                             #(into #{} (map stem %)))
        lex   (tw/make-pn-lexicon :bing-liu)
        ->pn  #(case (.retrieveValue lex %)             ; Lexicon lookup for P/N rules
                 "positive"  "P"
                 "negative"  "N"
                 "not_found" nil)
        ->scr #(let [term (stem %)]                     ; Match terms for other Sentiment Composite Rules
                 (reduce (fn [acc [scr terms]]
                           (if (contains? terms term)
                               (conj acc scr)
                               acc))
                         #{}
                         exprs))]
    (reset! Examples
            (reduce (fn [acc ^Instance inst]
                      (let [id    (long (.value inst COL-ID))
                            pairs (map #(str/split % #"_" 2)
                                        (str/split (.stringValue inst COL-TEXT) #" "))
                            poss  (map first  pairs)
                            terms (map second pairs)
                            rules (map #(if %1 (conj %2 %1) %2)
                                        (map ->pn  terms)       ; Single P|N rule or nil
                                        (map ->scr terms))]     ; Sequence of match-term rules
                       (assoc acc id {:pos-tags poss
                                      :rules    rules
                                      :polarity (polarize inst)})))
                    {}
                    (enumeration-seq (.enumerateInstances insts))))

    ;; Just tell them how many we have now
    (count @Examples))))



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
  "Returns a map of the IDs of tweets with positive polarities and those with
  negative polarities.  The caller may optionally specify a prefix for easy
  insertion into a DL-Learner configuration file."
  ([]
  (pn-examples ONT-PREFIX))

  ([prefix]
  (let [
        tag     (str prefix (when prefix ":") TWEET-TAG)
        tagger  #(str tag %)
        ids     (reduce (fn [acc [id info]]
                          (update-in acc
                                     [(:polarity info)]
                                     #(conj % id)))
                        {:positive (sorted-set)             ; Collect IDs for pos/neg examples
                         :negative (sorted-set)}
                        @Examples)
        xmps    (update-values ids #(map tagger %))]        ; Prefix % tag pos/neg IDs

    ;; Save P/N Text to pull in for DL-Learner runs
    (spit (str "resources/emo-sa/pn-examples.edn")
          (pr-str xmps))

    xmps)))



;;; --------------------------------------------------------------------------
(defn run
  "Runs a DL-Learner session to determine equivalent classes for Positive Texts."
  []
  :todo)

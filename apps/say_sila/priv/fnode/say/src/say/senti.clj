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
            [say.config         :as cfg]
            [say.log            :as log]
            [say.cmu-pos        :as pos]
            [say.dllearner      :as dll]
            [say.dolce          :as dul]
            [say.survey         :as six]
            [say.tweebo         :as twbo]
            [weka.core          :as weka]
            [weka.dataset       :as dset]
            [weka.tweet         :as tw]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp pprint]]
            [defun.core         :refer [defun]]
            [tawny.english      :as dl]
            [tawny.query        :as qry]
            [tawny.read         :as rd]
            [tawny.reasoner     :as rsn]
            [tawny.repl         :as repl]                           ; <= DEBUG
            [tawny.owl          :refer :all])
  (:import  (java.util Random)
            (org.semanticweb.HermiT Configuration
                                    Configuration$TableauMonitorType
                                    Prefixes
                                    Reasoner)
            (org.semanticweb.HermiT.monitor TableauMonitorAdapter)
            (org.semanticweb.HermiT.tableau BranchingPoint
                                            Tableau)
            (org.semanticweb.owlapi.model OWLOntology)
            (org.semanticweb.owlapi.reasoner InferenceType)
            (weka.core Attribute
                       DenseInstance
                       Instance
                       Instances)
            (weka.filters.unsupervised.attribute Reorder)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)
(rsn/reasoner-factory :hermit)

(def ^:const ONT-ISTUB  "http://www.dendrown.net/uqam/say-senti")
(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-senti.owl#")
(def ^:const ONT-FSTUB  "resources/KB/say-senti")
(def ^:const ONT-FPATH  "resources/KB/say-senti.owl")
(def ^:const ONT-PREFIX "senti")
(def ^:const DATASET    "resources/emo-sa/Sentiment140/sentiment-analysis.csv")
(def ^:const COUNTS     {:Sentiment140  1577278
                         :Kaggle        1349})
(def ^:const ARFFs      {:base          "resources/emo-sa/sentiment-analysis.arff"
                         :Kaggle        "resources/emo-sa/Sentiment140/sentiment-analysis.Kaggle.arff"
                         :Sentiment140  "resources/emo-sa/Sentiment140/sentiment-analysis.Sentiment140.arff"
                         :weka          "resources/emo-sa/Sentiment140/sentiment-analysis.Sentiment140.weka.arff"})

(def ^:const PREFIXES   {"senti"    ONT-IRI
                         "pos"      pos/ONT-IRI})

(def ^:const SPLIT-TAGS [:train :test])
(def ^:const TWEET-TAG  "t")                        ; Tweet individual have this tag plus the ID ( "t42" )

(defonce Columns        (dset/columns :s))          ; Weka format for Say-Sila Erlang feed
(defonce Columns-Film   (dset/columns :s00))        ; Weka format for Sentiment140/Kaggle datasets


;;; --------------------------------------------------------------------------
;;; TODO: Automate solution handling
(def ^:const Soln-Log       "resources/emo-sa/say-senti.solutions.l")
(def ^:const Soln-Without   #"\bThing\b")


;;; --------------------------------------------------------------------------
;;; Default values for configuration elements
(def ^:const INIT-NUM-EXAMPLES  100)
(def ^:const INIT-DATA-TAG      :Sentiment140)
(def ^:const INIT-DATA-SPLIT    {:datasets 10, :train 500, :test 500, :parts 10 :rand-seed 1})
(def ^:const INIT-LEX-TAG       :nrc)
(def ^:const INIT-LEARN-CAP     1)
(def ^:const INIT-TARGET        "sentiment")

; FIXME: The text index in our ARFF doesn't jive with the :emote config (used w/ gender & hierarchy)
(def ^:const INIT-TEXT-INDEX    2)                  ; 1-based index in ARFF


;;; --------------------------------------------------------------------------
;;; Collection of inference types to test timings on Reasoner
(defonce Inferences (into-array [InferenceType/CLASS_HIERARCHY
                                 InferenceType/CLASS_ASSERTIONS
                                 InferenceType/OBJECT_PROPERTY_HIERARCHY
                                 InferenceType/DATA_PROPERTY_HIERARCHY
                                 InferenceType/OBJECT_PROPERTY_ASSERTIONS   ; <- say-senti's gotcha!
                                 InferenceType/DATA_PROPERTY_ASSERTIONS
                                 InferenceType/SAME_INDIVIDUAL]))

(defonce ^:dynamic *build-ontology* nil)    ; Rebound to create ontologies with learned rules


;;; --------------------------------------------------------------------------
;;; Generally we'll be using sila/World, but we've got the same setup for
;;; local experimentation with Sentiment Composition Rules (SCR).
(defonce SCR            (atom {:examples {}
                               :ontology {}}))

(defonce Expressions    (if (cfg/?? :senti :use-scr?)
                            ;; Word sets which invoke Sentiment Composition Rules
                            {"NEGATION"     #{"not"}
                            ;"DECREASE-N"   #{"alleviate" "avoid" "handle" "lessen" "mitigate" "relieve"
                            ;                 "resolve" "soothe" "subside" "waive"}

                            ;"DECREASE-P"   #{"lack" "lose" "omit" "miss"}
                            ;"INCREASE-N"   #{"burst" "climb" "escalate" "intensify"
                            ;                ; 2-grams: "go up" "grow" "mark up" "pile up"
                            ;                }
                            ;"INCREASE-P"   #{"elevate" "enlarge" "expand" "extend" "increase" "progress"
                            ;                 "raise" "return" "rise" "soar" "surge"
                            ;                ; 2-grams: "be up" "build up" "come back"
                            ;                }
                            }
                            ;; Disable all Rules
                            {}))



;;; --------------------------------------------------------------------------
(defontology say-senti
  :iri     ONT-IRI
  :prefix  ONT-PREFIX
  :comment "Ontology for training sentiment models.")

;;; Bring in the CMU Part-of-Speech definitions ontology.
;;;
;;; NOTE: We're accessing DUL via the cmu-pos import, which will in turn import
;;;       either the full DOLCE+DnS Ultralite foundational ontology or our own
;;;       scaled-down say-dolce ontology as specified in the Say-Sila configuration.
(owl-import pos/cmu-pos)

;;; NOTE: Our model was initially based off work by Salguero and Espinilla \cite{salguero2016}.
;;;       They use the disjoint base classes: Term, Sentence, Document where Token and Punctuation
;;;       are subclasses of Term.  In our model, Punctuation is covered in cmu-pos, which leaves
;;;       Term and Token equivalent.  Additionally, following their model of disjoint classes here
;;;       causes a complexity explosion in the reasoner tableau.
(defclass Text
  :super    dul/InformationObject
  :disjoint pos/Token
  :label    "Text"
  :comment  "An Information Object consisting of text.")

;;; Keep the actual tweet content as a development aid
(defaproperty TextualContent)


;; DL-Learner isn't handling Pos/Neg Text subclasses well
(when (cfg/?? :senti :pos-neg?)
  (as-subclasses Text
    :disjoint
    (defclass NegativeText
      :label "Negative Text"
      :comment "A Text which expresses sentiment of a negative polarity")

    (defclass PositiveText
      :label "Positive Text"
      :comment "A Text which expresses sentiment of a positive polarity.")))


;;; --------------------------------------------------------------------------
;;; A Survey may be used to compare w/ analysis methods on social media
;;;
;;; TODO: Survey ontology elements should really be in say-sila, but
;;;       the actual processing is happening in say-sent.
(defclass Survey
  :super   dul/InformationObject
  :label   "Survey"
  :comment "A series of questions intended to extract information from a group of people")

(defindividual sassy
  :type  Survey
  :label "SASSY"
  :comment "Six Americas Short SurveY")

(defindividual six36
  :type  Survey
  :label "Six Americas 36-Question Survey"
  :comment "Original Six Americas survey with 36 questions.")

(defonce Surveys        (select-keys {:sassy sassy                  ; Only configured surveys
                                      :six36 six36}
                                     (cfg/?? :senti :surveys #{})))
(defonce Survey-Names   (map name (keys Surveys)))


;;; --------------------------------------------------------------------------
(defclass Affect
  :super    dul/Concept
  :label    "Affect"
  :comment  "A concept describing human sentiment or emotion.")

(as-subclasses Affect
  (defclass Positive
    :label   "Positive"
    :comment "An affective concept representing positive sentiment polarity.")

  (defclass Negative
    :label   "Negative"
    :comment "An affective concept representing negative sentiment polarity."))

(run! #(defpun %) [Positive Negative])


;;; Are we using specialized object properties?
(defoproperty-per (cfg/?? :senti :oproperties?)
  denotesAffect :super   dul/expresses
                :label   "denotes affect"
                :domain  pos/Token
                :range   Affect
                :comment "A relationship between a Token and the affect it expresses.")


(defmacro defemotion
  "Adds a Concept reprenting an emotion to the say-senti ontology"
  [emo sys & combos]
  (let [ename   `(name '~emo)
        descr   `(str "A concept which expresses the class of human affect generally known as " (str/lower-case ~ename)
                       " according to the system of base emotions by " (str/capitalize (name ~sys))
                       "."
                       (when-not (empty? '~combos)
                         (apply str " This emotion is a combination of the following base emotions: "
                                    (interpose ", " '~combos))))]

    `(do (defclass ~emo :super Affect, :label ~ename, :comment ~descr)
         (defpun ~emo))))


(defmacro defemotions
  "Adds all the emotions handled by a lexicon into the say-senti ontology."
  [sys]
  (let [esys  (eval sys)
        emote (fn [e] `(defemotion ~e ~esys))]
    (when esys
      ;; Create Affect concepts according to the system
      (conj (case esys
              :plutchik (map emote '[Anger Fear ,, Sadness Joy        ,, Surprise Anticipation ,, Disgust Trust])
              :ekman    (map emote '[Anger Fear    Sadness Happiness     Surprise                 Disgust])
              `((log/fmt-error "Unsupported emotion system: [~a]" ~esys)))

            `(log/fmt-info "Creating base emotion set: [~a]" ~esys)       ; Building a do-expr in reverse!
            'do))))

;;; Create Emotions in the ontology per the configured emo-system
(defonce Emotion-System (cfg/?? :senti :system))
(defemotions Emotion-System)

(defonce Affect-Fragments   (into {} (map #(let [a (iri-fragment %)]
                                             [(lower-keyword a) a])
                                           (rsn/instances Affect))))
(defonce Affect-Names       (into #{} (vals Affect-Fragments)))


;;; We must declare the different types of Aspect to be disjoint for the reasoner
;;; to handle equivalency classes based on the complement of a given Aspect.
;;;
;;; TODO: How do we want to handle secondaries wrt disjointness?
(apply as-subclasses Affect :disjoint (map #(owl-class %) Affect-Names))


;;; --------------------------------------------------------------------------
;;; Are we including secondary emotions?
(defonce Secondaries
    (when (cfg/?? :senti :secondaries?)
      (log/info "Creating secondary emotions for" (name Emotion-System) "system")
      (case Emotion-System
        :plutchik
          '{Aggressiveness [Anticipation Anger]
            Contempt       [Anger        Disgust]
            Remorse        [Disgust      Sadness]
            Disapproval    [Sadness      Surprise]
            Awe            [Surprise     Fear]
            Submission     [Fear         Trust]
            Love           [Trust        Joy]
            Optimism       [Joy          Anticipation]}
        (log/warn "No secondary emotions defined"))))


(defmacro defsecondaries
  "Defines secondary emotions (if we have any) in the say-senti ontology."
  [sys]
  (conj (map (fn [[sec [pr1 pr2]]]
         `(defemotion ~sec ~sys ~pr1 ~pr2))
        Secondaries)
        `do))
(defsecondaries Emotion-System)


(defn- add-epath
  "For a given pair of emotions (e1 e2), Adds a path to the accumulator
  map (acc) to symbolize that encountering e1 and then e2 leads to the
  specified secondary emotion (sec).  This function is used to create
  a map of maps to store these paths {e1->{e2->sec}}.  This function
  should be called twice with each base emotion as e1."
  [acc sec [e1 e2]]
  (conj acc [e1 (assoc (get acc e1) e2 sec)]))


;;; Hierarchical hashmap to follow two primary emotion symbols to a secondary emotion symbol
(defonce Dyad-Combos (reduce (fn [acc [sec pair]]
                               ;; Chain calls for {e1->{e2->sec}} and {e2->{e1->sec}}
                               (add-epath (add-epath acc sec pair)
                                          sec
                                          (reverse pair)))
                             {}
                             Secondaries))


;;; We need a string-based version of Dyad-Combos to identify secondaries in Texts
(defn- kvsym->kvstr
  "Converts a hierarchical sym->sym hashmap to the equivalent lowercase str->str hashmap."
  [[k v]]
  ;; KEY is sym->str, VALUE may be an inner hashmap
  [(str k) (if (map? v)
               (into {} (map kvsym->kvstr v))   ; Recurse to follow path
               (str v))])                       ; Final (leaf) value

(defonce Dyad-Emotion-Paths (into  {} (map kvsym->kvstr Dyad-Combos)))
(defonce Dyad-Names         (into #{} (map name (keys Secondaries))))


(defn affect?
  "Returns true if the named concept is the name of a sentiment or emotion
  defined in this ontology."
  [concept]
  (or (contains? Affect-Names concept)
      (contains? Dyad-Names concept)))


;;; --------------------------------------------------------------------------
;;; Sentiment Composition Rules (SCR):
;;; \ref{bing2015}
;;;
;;; Note that we will be creating individual say-senti-RULE ontologies which
;;; include the individuals (Texts) from the training corpus which express
;;; a given rule.  However, it is in this, the main say-senti ontology, that
;;; all the SCRs are defined.
(defclass SentimentCompositionRule
  :super   dul/Concept
  :label   "Sentiment Composition Rule"
  :comment (str "An abstraction describing a Text or portion of a Text according to its
                 positive or negative contribution to the polarity of that Text."))


;;; Are we using specialized object properties?
(defoproperty-per (cfg/?? :senti :oproperties?)
  indicatesRule :super   dul/expresses
                :label   "indicates rule"
                :domain  pos/Token
                :range   SentimentCompositionRule
                :comment "A relationship between a Token and the sentiment composition rule it expresses.")


(defmacro defscr
  "Adds a Sentiment Composition Rule (component) subclass to the say-senti ontology"
  [tag descr]
  `(do (defclass ~tag
         :super   SentimentCompositionRule
         :label   (str "Sentiment Composition Rule - " (name '~tag))
         :comment ~descr)
       (defpun ~tag)))

;; FIXME Reclassify P/N and emotions if we're not going to use other Liu's SCRs
(defscr P "An atomic, nonterminal sentiment composition expressing positive sentiment.")
(defscr N "An atomic, nonterminal sentiment composition expressing negative sentiment.")

(when (cfg/?? :senti :use-scr?)
  ;; TODO: Verify in \liu2015 if P/N refer to a pos/neg modifier or a pos/neg aspect
  (log/notice "Creating Sentiment Composition Rules")
  ;(defscr DECREASE-N  "Expressions which decrease NPI and NE terms.")
  ;(defscr DECREASE-P  "Expressions which decrease PPI and PO terms.")
  ;(defscr INCREASE-N  "Expressions which increase NPI and NE terms.")
  ;(defscr INCREASE-P  "Expressions which increase PPI and PO terms.")
  (defscr NEGATION    "Expressions which negate other terms.")

  (defclass AffectNegator
    :super    pos/Token
    :label    "Affect Negator"
    :equivalent (dl/and pos/Token
                        (dl/some indicatesRule NEGATION)
                        (dl/some dul/directlyPrecedes (dl/some denotesAffect Affect))))

  (defoproperty negatesAffect
    :super    dul/expresses
    :label    "negates affect"
    :domain   AffectNegator
    :range    Affect))



;;; --------------------------------------------------------------------------
;;; Support for TweeboParser dependency trees?
(when (cfg/?? :senti :use-tweebo?)

  (defclass Coordination
    :super   dul/Concept
    :label   "Coordination"
    :comment "A linguistic Concept that links one or more Conjuncts.")

  (defclass Conjuncts
    :super   dul/Collection
    :label   "Conjuncts"
    :comment "A Collection that contains the set of conjuncts conjoined with a conjunction or coordination.")

  (defclass MultiWordExpression
    :super   dul/Collection
    :label   "Multi-word concept"
    :comment "A Collection of Tokens that together represent a single unit of meaning.")

  ;; FIXME: dependsOn will need to be under associatedWith, depending on the hierarchy config setting
  (defoproperty dependsOn
    :domain  dul/Entity
    :range   dul/Entity
    :label   "depends on"
    :comment "A relationship describing how one Entity's existence or correctness is contingent on another."
    :characteristic :transitive))


;;; --------------------------------------------------------------------------
;;; Tell DL-Learner about our ontology elements
(dll/register-ns)


;;; --------------------------------------------------------------------------
(defn primaries->secondaries
  "Returns a sequence of tag indicating which datasets we use for evaluation."
  ([pris secs]
  (if-let [p1 (first pris)]
    (let [pris* (rest pris)
          secs* (reduce (fn [secs* [p2 s]]
                          (if (some #{p2} pris*)
                              (conj secs* s)
                              secs*))
                        secs
                        (get Dyad-Emotion-Paths p1))]
      (recur pris* secs*))
    secs))

  ([pris]
  (primaries->secondaries pris '())))



;;; --------------------------------------------------------------------------
(def     Polarity-Markers   {"Negative" (str log/Lt-Blue   "--")
                             "Positive" (str log/Lt-Yellow "++")
                             :?         (str log/White     "??")})

(def     Emotion-Colours    {"Anger"        log/Red1
                             "Fear"         log/Green3
                             "Joy"          log/Gold1
                             "Sadness"      log/Magenta
                             "Anticipation" log/Orange3
                             "Surprise"     log/Cyan1
                             "Trust"        log/SpringGreen1
                             "Disgust"      log/Magenta2})

(defn elegend
  "Returns a legend for emotion to colour mapping."
  []
  (map (fn [[emo col]]
         (str col emo log/Text))
       (merge Emotion-Colours
              Polarity-Markers)))


;;; --------------------------------------------------------------------------
(defn eword
  "Returns a printable colour-coded string of word high-lighted with respect
  to the specified sentiment/emotion set."
  ([word affect]
  (eword word affect nil nil))


  ([word affect survey]
  (eword word affect survey (tw/make-stemmer)))


  ([word affect survey sball]
  (let [smark (when (and survey (six/in-survey? survey word sball))
                log/Underline)]
    ;; Make sure we have something to process
    (if (every? empty? [affect smark])
      word
      (let [; Surround the word with colour-coded pos/neg markers
            [prefix
             suffix] (reduce (fn [[pri suf :as acc]
                                  [pole code]]
                               (if (contains? affect pole)
                                   [(str code pri)
                                    (str suf code)]
                                   acc))
                             ["" ""]
                             Polarity-Markers)
            ; Colourize the letters in the word according to the emotions
            colours (vals (select-keys Emotion-Colours affect))
            ccnt    (count colours)
            weave   (if (zero? ccnt)
                      (str smark word)
                      ;; Pad word for an even split across emotions
                      (let [wlen    (count word)
                            csize   (Math/ceil (/ wlen ccnt))
                            padding (take (- (* ccnt csize) wlen)       ; Extra chars required
                                          (repeat \*))                  ; Asterix, not Obelix
                            [pada
                             padz]  (split-at (quot (count padding) 2) padding) ; Extra @ end: *word**
                            chunks  (partition-all csize (concat pada
                                                                 (seq word)
                                                                 padz))]
                        ;; Weave emotion colours into word!
                        (apply str (flatten (interleave (repeat smark) colours chunks)))))]

        ;; End the colourized word must by going back to no colour
        (str prefix log/Text
             weave  log/Text
             suffix log/Text))))))



;;; --------------------------------------------------------------------------
(defn etweet
  "Returns a colourized string representing the example tweet. Survey keyword
  hits are underlined.  The function defaults to using the configured survey,
  or SASSY if none is configured.)"
  ([xmp]
  ;;; TODO: Handle multiple surveys if we're going to support more than one
  (etweet xmp (first (cfg/?? :senti :surveys [:sassy]))))


  ([{:keys [tid
            polarity
            content
            analysis]}
    survey]
  (let [snowball  (when survey
                    (tw/make-stemmer))
        colourize (fn [[word affect]]
                    (eword word affect survey snowball))
        pn-code   (Polarity-Markers (Affect-Fragments polarity polarity))]
    ;; The analysis includes sentiment, emotion, and SCRs. The latter are ignored.
    (apply str (interpose \space
                          (conj (map colourize (zip content analysis))  ; Mark affect
                                (log/<> tid pn-code)))))))              ; Tag tweet



;;; --------------------------------------------------------------------------
(defn eprint-scr
  "Prints colourized representions of the SCR tweets for the specified data tag."
  [dtag]
  (run! #(println (etweet %)) (-> @SCR :examples dtag)))



;;; --------------------------------------------------------------------------
(defn enumerate-dataset
  "Returns a sequence of tag indicating which datasets we use for evaluation."
  [ds n]
  (weka/tag-filename ds (strfmt "~3,'0d" n)))



;;; --------------------------------------------------------------------------
(defn key-prng
  "Returns a keyword representing the specified PRNG seed, or if no seed is
  specified, the configured rand-seed for the senti data-split."
  ([]
  (let [{:keys [rand-seed]} (cfg/?? :senti :data-split INIT-DATA-SPLIT)]
    (key-prng rand-seed)))

  ([seed]
  (keyize :r seed)))



;;; --------------------------------------------------------------------------
(defn which-datasets
  "Returns a sequence of tag indicating which datasets we use for evaluation."
  []
  (into #{} (remove #{:base} (keys ARFFs))))



;;; --------------------------------------------------------------------------
(defn which-data
  "Returns a data tag indicating which dataset has been requested from among
  the specified options"
  [& opts]
  (if-let [dtag (some (which-datasets) opts)]
    dtag
    INIT-DATA-TAG))



;;; --------------------------------------------------------------------------
(defn which-arff
  "Returns the filepath of the ARFF intended for testing.

  TODO: We build multiple test ARFFs; we need to use them test."
  [tt & opts]
  (let [dtag (apply which-data opts)
        rtag (key-prng)]
    (weka/tag-filename (ARFFs dtag)
                       (apply str (map name [rtag "." tt])))))



;;; --------------------------------------------------------------------------
(defn which-target
  "Returns the name of the target attribute in training/testing datasets."
  []
  (cfg/?? :emote :target INIT-TARGET))



;;; --------------------------------------------------------------------------
(defn set-num-examples!
  "Defines the number of examples we should use from the source tweet data."
  [n]
  (cfg/!! :senti :num-examples n))



;;; --------------------------------------------------------------------------
(defn ont-iri
  "Creates a (String) IRI for the specified Sentiment Composition Rule (SCR)."
  [rule]
  (str ONT-ISTUB "-" (name rule) ".owl#"))



;;; --------------------------------------------------------------------------
(defn ^OWLOntology make-ontology
  "Creates a version (copy) of the say-senti ontology, intended to include
  individuals expressing the specified Sentiment Composition Rule (SCR)"
  ([rule]
  (make-ontology rule nil))


  ([rule learned]
  (let [scr    (name rule)
        prefix #(apply str % "-" scr %&)
        ;; We use a (sub)ontology to hold the texts and DL-Learner solutions
        ont    (ontology
                 :tawny.owl/name (prefix "say-senti")
                 :iri     (ont-iri scr)
                 :prefix  (prefix "scr")
                 :import  say-senti
                 :comment (str "Ontology for training sentiment models wrt. the Sentiment Composition Rule " scr))

        ;; Create a parent class for output representations as described by DL-Learner
        dltext (owl-class ont (dll/name-learned)
                 :super    Text
                 :label    "Learned Positive Text"
                 :comment  (str "A Text representing a candidate formula for determining if a given Text "
                                "expresses positive sentiment."))

        add-dl (fn [n expr]
                 (log/fmt-info "LEARNED<~a>: ~a" n expr)
                 (owl-class ont (dll/name-learned n)
                   :super dltext
                   :equivalent (eval expr))
                 (inc n))]

    ;; If we've got learned expressions, create corresponding classes to describe Postive Texts
    (when learned
      (binding [*build-ontology* ont]
        (reduce add-dl 1 learned)))

    ont)))



;;; --------------------------------------------------------------------------
(defn learned-positive-texts
  "Returns a set of LearnedPositiveText individuals from the specified
  ontology."
  [ont]
  (binding [rsn/*reasoner-progress-monitor* (atom rsn/reasoner-progress-monitor-silent)]
    (let [rsnr    (rsn/reasoner ont)
          learned (owl-class ont (dll/name-learned))              ; DL-Learner equivalent soln
          ptexts  (rsn/instances ont learned)]                    ; Predicted positive texts

      (log/debug "Learned:" (map qry/tawny-name (rsn/isubclasses ont learned)))

      ;; Make sure HermiT doesn't hoard memory.  Tawny-OWL (as of version 2.0.3) is
      ;; not calling dispose on the HermiT reasoner due to crashiness they've seen.
      (.dispose rsnr)
      (rsn/discard-reasoner ont)

      ptexts)))



;;; --------------------------------------------------------------------------
(defprotocol Polarizer
  "Determines negative|positive polarity for various datatypes."
  (polarize [x] "Return the sentiment polarity as :positive or :negative."))

(extend-protocol Polarizer
  Number
  (polarize [x]
    ;(log/debug "polarize:" x)
    (if (Double/isNaN x)
      :?
      (if (<= x 0.0)
          :negative
          :positive)))

  Instance
  (polarize [inst]
    (if (.classIsMissing inst)
        :?
        (polarize (.classValue inst))))


  String
  (polarize [pn]
    (polarize (Integer/parseInt pn))))



;;; --------------------------------------------------------------------------
(defn label-polarity
  "Returns the String «pos» or «neg» according to the polarity of x."
  [x]
  (label-polarity [x]
    (case (polarize x)
      :negative "neg"
      :positive "pos")))



;;; --------------------------------------------------------------------------
(defprotocol Labeller
  "Creates labels for say-senti purposes."
  ;; TODO: label-polarity [from above]
  (label-text [x] "Returns a String identifier for a Text (tweet)."))

(extend-protocol Labeller

  Instance
  (label-text [inst]
    (label-text (.value inst (int (Columns :id)))))

  String
  (label-text [s]
    ;; Allow multiple calls without retagging
    (if (str/starts-with? s TWEET-TAG)
        s
        (str TWEET-TAG s)))

  Object
  (label-text [x]
    (str TWEET-TAG (longify x))))



(defn label-text-token
  "Returns a String identifier for a Text (tweet) and the token number.

  Examples:
    - for the tenth token on the 42nd tweet, the label will be: t42-10
    - and for a MWE (multi-word expression) role on that token: t42-mwe10"
  ([txt tok]
  (hyphenize (label-text txt) tok))


  ([txt tok role]
  (hyphenize (label-text txt) (str (str/lower-case (name role)) tok))))



;;; --------------------------------------------------------------------------
(defn- text-type
  "Determines the actual ontology class that should be assigned to a textual
  individual."
  [pos-neg? polarity]
  ;; Do we want to explicitly represent positive/negative texts?
  (if pos-neg?
      ;; If the data has a polarity label, use the pos/neg Text subclass
      (case polarity
        :negative NegativeText
        :positive PositiveText
                  Text)
      ;; No configured override
      Text))



;;; --------------------------------------------------------------------------
(defn- express
  "Asserts a role relation in the specified ontology of either dul:express
  or one of its sub-properties."
  [ont ttid token concept]
  (let [prop (if (affect? concept)  ; NOTE: depending on the oproperties? config setting,
                 denotesAffect      ;       both properties may collapse to dul/express,
                 indicatesRule)]    ;       but this will not hurt functionality.

    (refine ont token :fact (is prop (individual say-senti concept)))))



;;; --------------------------------------------------------------------------
(defn add-text
  "Adds a textual individual to the specified ontology.  The default behaviour
  is to create a new individual of type Text, but the arity-4 clause allows
  the caller to specify any entiy needing to represent a series of Tokens."
  ([ont tinfo]
  (add-text ont tinfo (cfg/? :senti)))


  ([ont tinfo sconf]
  (add-text ont nil tinfo sconf))


  ([ont entity
    {:keys [account analysis content tid polarity pos-tags surveys]}            ; Text breakdown
    {:keys [full-links? links? pos-neg? secondaries? use-scr? use-tweebo?]}]    ; Senti-params
  ;; The code will assume there's at least one token, so make sure!
  (when (seq pos-tags)
    (let [msg   (apply str (interpose " " content))
          text  (or entity
                    (individual ont tid                             ; Entity representing the text
                      :type (text-type pos-neg? polarity)))]        ; Determine textual type

    ;; Annotate the actual text content as a development aid
    (refine ont text :annotation (annotation TextualContent msg))

    ;; If they didn't pass an entity, assume this is a tweet
    ;; TODO: Handle access to say.sila namespace
    (when (and account                                              ; Test data may not have screen names
               (not entity))
      (refine ont (individual ont account) :fact (is (object-property ont "publishes") text)))

     ;; Prepare for Tweebo Parsing if desired
     (when use-tweebo?
       (twbo/prepare tid msg))

      ;; And entities for each of the terms, linking them together and to the text
      (reduce
        (fn [[cnt tokens :as info]
             [checks tag word svys]]
          ;; Get the Part of Speech for the tag reported by Weka
          (if-let [pos (pos/lookup# tag)]

            ;; Set up an individual for this Token
            (let [ttid (str tid "-" cnt)
                  curr (individual ont ttid
                                   :type  pos/Token
                                   :label (str ttid " (" tag ")"))]

              ;; Link Token to the original Text and set POS Quality
              (refine ont text :fact (is dul/hasComponent curr))
              (refine ont curr :fact (is pos/isPartOfSpeech pos))

            ;; Link tokens to each other
            (when-let [prev (first tokens)]

              ;; Are we including the Token ordering?
              (when links?
                (refine ont curr :fact (is dul/directlyFollows prev))

                ;; The reasoner can figure out the rest, but being explicit may be faster
                (when full-links?
                  ;; The current Token comes after all the tokens we've seen so far
                  (refine ont prev :fact (is dul/directlyPrecedes curr))
                  (run! (fn [tok]
                          (refine ont curr :fact (is dul/follows tok))
                          (refine ont tok  :fact (is dul/precedes curr)))
                        tokens)))

              ;; TODO: This is SCR prototypical code.  Integrate it into the module if it's successful.
              (when use-scr?
                (binding [*ns* (find-ns 'say.senti)]
                  (when (= 'NEGATION (check-fact prev indicatesRule))
                    (doseq [aff (filter affect? checks)]
                      (log/debug "Token" (rd/label-transform ont prev) "negates" aff)
                      (refine ont prev :fact (is negatesAffect (individual say-senti aff))))))))

            ;; Express sentiment composition rules
            (doseq [rule checks]
              (express ont ttid curr rule))

            ;; TODO: This is prototypical code for secondary emotions
            (when secondaries?
              (doseq [sec (primaries->secondaries checks)]
                ;(log/debug "Token" ttid "expresses dyad:" sec)
                (express ont ttid curr sec)))

            ;; TODO: Prototypical code for Six Americas experimental surveys
            (doseq [s svys]
                (refine ont curr :fact (is dul/isComponentOf (Surveys s))))

            ;; Continue the reduction
            [(inc cnt)
             (conj tokens curr)])

            ;; Ignored/invalid Part of Speech tag
            (do ;(log/fmt-debug "Ignoring POS tag '~a'" tag)
                info)))

        [1 nil]                             ; Acc: Token counter, reverse seq of tokens
        (zip analysis pos-tags content surveys))))))



;;; --------------------------------------------------------------------------
(defn add-dependencies
  "Incorporates a tweet's output from the TweeboParser into the specified
  ontology."
  ([ont {:keys [tid]
         :as   xmp}]
  (log/info "Finding dependencies for" tid)
  (add-dependencies ont xmp (twbo/predict tid)))


  ([ont
    {:keys [tid content pos-tags]}
    tweebo]
  (let [include #(refine ont %1 :fact (is dul/hasComponent %2))
        equiv?  #(or (= %1 %2)
                     (every? #{"\"" "QUOTE"} [%1 %2]))
        make    (memoize (fn [ling n]
                            (let [tokid  (label-text-token tid n)
                                  token  (individual ont tokid)
                                  entid  (label-text-token tid n ling)
                                  entity (individual ont entid
                                           :type (case ling "CONJ"  Conjuncts
                                                            "COORD" Coordination
                                                             "MWE"  MultiWordExpression))]
                              ;; Add the relation for token-->entity to the ontology.
                              ;; The Tweebo map entry for Token N does not reference the entity.
                              (log/debug "Adding" ling entid)
                              (refine ont token :fact (is dul/expresses entity))

                              ;; Multi-word expression roots (n) don't have the MWE code
                              (when (= ling "MWE")
                                (include entity token))

                              ;; We will only touch the ontology once as we are memoized
                              entity)))]

    ;; Run through our example and the Tweebo parse, token by token
    (loop [[tok1                              & content*]   content     ; Tweet tokens
           [pos1                              & pos-tags*]  pos-tags    ; Parts of Speech
           [[sub tok2 _ pos2 pos3 _ obj ling] & tweebo*]    tweebo]     ; Tweebo output
      ;; All three arguments should be in alignment, except tweebo may have a final [""]
      (when pos1
        ;; Complain if the POS analysis doesn't match up (uncommon)
        (when (not= pos1 pos2 pos3)
            (log/fmt-warn "Part-of-speech mismatch on ~a: token[~a/~a] pos[~a~a~a]"
                          tid tok1 tok2 pos1 pos2 pos3))
        (if (equiv? tok1 tok2)
          (do
            ;; We're looking from the leaf (subject) up to the parent node (object).
            ;; -1 : subjet token is uninteresting per Tweebo
            ;;  0 : subjet token is a root node
            ;;  N : subjet token depends on the Nth token (object)
            (when (pos? (Long/parseLong obj))
              (let [[subid   objid]  (map #(label-text-token tid %) [sub obj])  ; t99-9
                    [subject object] (map #(individual ont %) [subid objid])]   ; Tokens

              ;; Add dependency relation to ontology
              ;(log/debug subid  "dependsOn" objid)
              (refine ont subject :fact (is dependsOn object))

              ;; Handle linguistic entities: Conjuncts, Coordinations and Multi-word expressions
              (when-let [entity (and (not= ling "_")
                                     (make ling obj))]
                (include entity subject))))

              ;; Move on to the next token
              (recur content* pos-tags* tweebo*))

            ;; Abort!  The parsers disagree wrt tokenization.
            (log/fmt-error "Text/tweebo mismatch on ~a: token[~a/~a] pos[~a~a~a]"
                           tid tok1 tok2 pos1 pos2 pos3)))))))



;;; --------------------------------------------------------------------------
(defn read-solutions
  "Returns a vector of candidate solutions that were probably previously
  stored using process-solutions."
  ([]
  (read-solutions Soln-Log))


  ([fpath]
  (when (.exists (io/file fpath))
    (log/info "Reading DL-Learner solutions:" fpath)
      (with-open [rdr (io/reader fpath)]
         (seq (reduce (fn [solns txt]
                        (if-let [s (dll/read-solution txt)]
                          (conj solns s)
                          solns))
                      []
                      (line-seq rdr)))))))



;;; --------------------------------------------------------------------------
(defn cap-solutions
  "Selects a the best solutions from the specified sequence."
  ([solns]
  (cap-solutions solns (cfg/?? :senti :learn-cap INIT-LEARN-CAP)))


  ([solns cap]
  ;; Keep the solutions with the elements we want and remove those we don't
  (let [score   #(let [m (meta %)]      ; Scores are in the meta data
                   (or (:f1 m)          ; Prefer F-measure [CELOE and OCEL]
                        (:acc m)))      ; Fallback to accuracy [OCEL only]
        better  #(compare (score %2)    ; Sort high to low
                          (score %1))]

    ;; Cap at a maximum number of solutions,
    (take cap (sort better solns)))))



;;; --------------------------------------------------------------------------
(defn save-ontologies
  "Saves the say-senti ontology and all the SCR ontologies in OWL format."
  []
  (save-ontology say-senti ONT-FPATH :owl)
  (merge {:say-senti ONT-FPATH}
         (save-ontology-map (:ontology @SCR) ONT-FSTUB)))



;;; --------------------------------------------------------------------------
(defun ^OWLOntology populate-ontology
  "Populates the senti ontology using examples from the ARFFs.  The caller
  may specify an SCR identifier (keyword or string), rather than an ontology.
  If this is the case, the function will create the ontology for that rule.
  In either case function returns the ontology, populated with the new examples.
  The called may also specify a sequence of learned expressions that will be
  included in the ontology as subclasses to LearnedPositiveText."
  ([xmps :guard map?]
  (update-kv-values xmps #(populate-ontology %1 %2)))


  ([ont xmps]
  (populate-ontology ont xmps nil))


  ([ont xmps sconf :guard map?]
  (populate-ontology ont xmps nil sconf))

  ([ont xmps learned]
  (populate-ontology ont xmps learned (cfg/? :senti)))


  ([ont :guard #(or (keyword? %) (string?  %))
    xmps
    learned
    sconf]
  (log/info "Creating ontology:" ont)
  (populate-ontology (make-ontology ont learned) xmps learned sconf))


  ([ont xmps _ sconf]
  ;; Add positivity tokens if we're guiding learning (or testing the system)
  (run! #(add-text ont % sconf) xmps)

    ;; Add Tweebo dependencies
    (when (cfg/?? :senti :use-tweebo?)
      (twbo/wait)
      (run! #(add-dependencies ont %) xmps))

    ;; Remember that ontologies are mutable
    ont))



;;; --------------------------------------------------------------------------
(defn- populate-scr-ontologies
  "Populates the SCR ontologies using SCR examples from the ARFFs"
  ([xmps]
  (populate-scr-ontologies xmps false))


  ([xmps solns?]
  (let [sconf (cfg/? :senti)                ; Freeze the configuration while we work
        solns (when solns?
                (read-solutions))]
    ;; Create ontologies for each SCR, each populated with individuals expressing the rule
    ;; TODO: We're still using all the learned rules, instead of the capped solutions
    ;;       because we need any referenced rules, even if they didn't make the cut.
    (update-kv-values xmps #(populate-ontology %1 %2 solns sconf)))))




;;; --------------------------------------------------------------------------
(defn- create-pn-goal
  "Checks the :senti configuation and returns a map with the elements needed
  to construct datasets and populate ontologies.  The function allows the
  caller to override the configuration by adding :key value pairs as arguments."
 ([dset]
 (let [{:as   conf
        :keys [num-examples]
        :or   {num-examples INIT-NUM-EXAMPLES}} (cfg/? :senti)]
    ;; The default is the number of examples for creating ontology individuals
    (create-pn-goal dset num-examples conf)))


 ([dset cnt]
 (create-pn-goal dset cnt (cfg/? :senti)))


 ([dset cnt {:as   conf
             :keys [balance?]}]
 ;; Unless it's a singleton, odd counts that are balanced will have an extra instance
 (let [[goal
        checks] (if (and balance?
                         (> cnt 1))
                    [(int (/ cnt 2)) [:positive :negative]]     ; pos/neg instances separately
                    [cnt [dset]])]                              ; all instances together

   ;; Add what we need for our goals
   (assoc conf :goal    goal
               :checks  checks
               :dataset dset))))



;;; --------------------------------------------------------------------------
(defn- split-pn-goals
  "Returns a map of pos/neg creation goals for creating :train and :test
  datasets."
  [dset]
  (let [{:as   conf
         :keys [data-split]} (cfg/? :senti)]

    (into {} (map #(vector % (create-pn-goal dset (% data-split) conf))
                  SPLIT-TAGS))))



;;; --------------------------------------------------------------------------
(defn- describe-creation
  "Returns a string describing the creation goals."
  ([{:keys [balance?
            dataset
            extra-info
            goal]}]
   (str (if balance? (str "Balancing " goal "/" goal)
                     (str "Creating "  goal))
        " " (name dataset)
        (when extra-info
          (str " " extra-info))))


  ([goals tt]
  (describe-creation (assoc (goals tt)
                            :extra-info (name tt)))))



;;; --------------------------------------------------------------------------
(defn- creation-done?
  "Returns true if the callers creation activites have completed."
  [cnts
   {:keys [goal checks]}]
  (every? #(>= (cnts %) goal) checks))



;;; --------------------------------------------------------------------------
(defn- creation-full?
  "Returns true if a (pos|neg balanced) category has filled up during  the
  creation of a dataset or an example set."
  [cnts pole
   {:keys [balance? goal checks]}]
  (and (not= pole :?)                       ; Not under evaluation
       balance?
       (>= (cnts pole) goal)))



;;; --------------------------------------------------------------------------
(defn- zero-pn-counter
  "Returns a map used to initialize counting SCR examples or data instances."
  [dset]
  (reduce #(assoc %1 %2 0) {} [dset :positive :negative :?]))



;;; --------------------------------------------------------------------------
(defn- inc-pn-counter
  "Returns an updated map after incrementing the couter values for the dataset
  and the specified polarity."
  [cnts dset pn]
  (update-values cnts [dset pn] inc))



;;; --------------------------------------------------------------------------
(defrecord Toolbox [all-pn?         ; Use only texts with sentiment/emotion
                    stoic?          ; Determines if text is void of sentiment
                    stem            ; Find grammatical stem for a word
                    sense           ; Identify tokens with sentiment/emotion
                    scr             ; Identify tokens invoking sentiment composition rules
                    surveys])       ; Identify surveys where this word is a keyword


(defn toolbox
  "Creates and bundles utility functions used for processing textual examples
  with respect to sentiment/emotion content and sentiment composition rules.
  This function bundle is tuned by parameters in the :senti section of the
  configuration."
  []
  ;; Create a closure for a configuration-based analysis
  (let [all-pn? (cfg/?? :senti :skip-neutrals?)
        lex     (tw/make-lexicon (cfg/?? :senti :lexicon :liu)) ; TODO: Capture lex change on config update
        sball   (tw/make-stemmer)                               ; Weka Affective Tweets plus Snowball stemmer
        stem    (fn [w]
                  (.stem sball w))

        exprs   (update-values Expressions                      ; Pre-stem Liu's SCR expressions
                               #(into #{} (map stem %)))]

    ;; Bundle everything up
    (map->Toolbox
     {:all-pn?  (fn [] all-pn?)

      :stoic?   (fn [{:keys [analysis]}]                        ; Check that we're not including neutral Texts
                  (and all-pn? (every? empty? analysis)))       ; ..and that no sentiment (rule) is expressed

      :stem     stem

      :sense    #(tw/analyze-token+- lex % Affect-Fragments)    ; Lexicon lookup for P/N rules

      :scr      #(let [term (stem %)]                           ; Match terms for Sentiment Composite Rules
                   (reduce (fn [acc [scr terms]]
                             (if (contains? terms term)
                                 (conj acc scr)
                                 acc))
                           #{}
                           exprs))

      :surveys  #(reduce (fn [acc s]                            ; Link Six Americas surveys
                           (if (six/in-survey? s % sball)
                               (conj acc s)
                               acc))
                          #{}
                          (keys Surveys))})))



;;; --------------------------------------------------------------------------
(defn make-example
  "Processes raw data to create a hashmap representing an instance in
  'example' form, which is an intermediate structure in a conversion to
  a textual individual in an ontology."
  ([tools tid sname elements]
  (make-example tools tid sname elements :?))


  ([tools tid sname elements polarity]
  (let [pairs   (map #(str/split % #"_" 2)                      ; Separate elements: [PoS token]
                      (str/split elements #" "))

        terms   (map #(-> % (second)                            ; FIXME: Get terms using
                            (str/lower-case)                    ;  affective.core.Utils/tokenize
                            (.replaceAll "([a-z])\\1+" "$1$1")) ;  repeated letters
                      pairs)

        affect  (map (:sense tools) terms)           ; Affect: pos|neg|emo or nil per term
        rules   (map (:scr tools) terms)]            ; Set of match-term rules per term

    ;; Put all that together to build the example
    {:account   sname
     :tid       tid
     :polarity  polarity
     :content   terms
     :rules     rules
     :pos-tags  (map first pairs)
     :surveys   (map (:surveys tools) terms)
     :analysis  (map set/union affect rules)})))



;;; --------------------------------------------------------------------------
(defn instances->examples
  "When a dataset tag is specified (dset), this function returns a hashmap
  keyed by that tag plus any SCR tags which apply to one or more instances.
  The values of this hashmap are sets of hashmaps, where each (sub)hashmap
  represents an instance in 'example' form, which is an intermediate structure
  in a Text's conversion from Weka instance to ontology individual.

  When the dataset is not specified, the function only returns the set
  of example hashmaps corresponding to the specified instances."
  ([data]
  (:data (instances->examples :data data)))


  ([dset data]
  (let [insts (weka/load-dataset data (which-target))
        icnt  (.numInstances insts)]
    (log/fmt-debug "Text instances~a: ~a" dset icnt)
    (instances->examples dset insts icnt)))


  ([dset ^Instances insts cnt]
  ;; Keep track of how many examples to create, as per the configured 'balance' setting
  (let [[col-id
         col-sname
         col-text]  (map Columns [:id :screen_name :text])
        tools       (toolbox)
        stoic?      (:stoic? tools)
        goal        (create-pn-goal dset cnt)]

    ;; The number of examples we're creating depends on how things were configured
    (log/info (describe-creation goal)
              "SCR examples [pos/neg]"
              (if ((:all-pn? tools)) "(emotive)" "(includes stoic)"))

    ;; Shall we (pseudo)randomize the instances?
    (when-let [seed (cfg/?? :senti :rand-seed)]
      (log/fmt-info "Shuffling ~a input instances: seed[~a]" (.numInstances insts) seed)
      (.randomize insts (Random. seed)))

    ;; Throw away the counter & return the folded example sequence
    (second
      (reduce (fn [[cnts xmap :as info]                             ; FUN: add a textual eXample
                   ^Instance inst]
                ;(log/debug "Counts:" cnts)
                ;; Do we have enough examples to stop?
                (if (creation-done? cnts goal)
                  (do (log/info "Examples:" cnts)
                      (reduced info))
                  (let [tid    (label-text (.stringValue inst (int col-id)))
                        sname  (.stringValue inst (int col-sname))
                        pole   (polarize inst)
                        elms   (.stringValue inst (int col-text))       ; Text elements are "pos_term"
                        xmp    (make-example tools tid sname elms pole) ; Example as a hashmap
                        xkeys  (apply set/union #{dset} (xmp :rules))]  ; Full dataset & all SCRs
                    ;; Do we skip|process this Text??
                    (if (or (stoic? xmp)                                ; Is it void of pos/neg/emotion?
                            (creation-full? cnts pole goal))            ; Still collecting for this polarity?
                      info
                      [(inc-pn-counter cnts dset pole)                  ; Update pos/neg/all counts
                       (update-values xmap xkeys #(conj % xmp))]))))

            [(zero-pn-counter dset)                                 ; ACC: total/pos/neg counts
             (reduce #(assoc %1 %2 #{}) {} (keys Expressions))]     ;      Examples keyed by rule

            (enumeration-seq (.enumerateInstances insts)))))))      ; SEQ: Weka instances



;;; --------------------------------------------------------------------------
(defn report-examples
  "Give positive/negative coverage and sentiment statistics for sets of
  intermediate-format examples or a hashmap with multiple sets of examples
  as values."
  ([xmps]
  (if (map? xmps)
      (run! #(apply report-examples %) xmps)        ; Report keyed example sets
      (report-examples :examples xmps)))            ; Single set of examples


  ([dtag xmps]
  (let [;; Statistics on text polarity and presence of affect
        stats   (reduce #(let [ss (if (every? empty? (:analysis %2))
                                      :stoic
                                      :senti)]
                           (update-values %1 [:count (:polarity %2) ss] inc))
                        (zero-hashmap :count :positive :negative :? :senti :stoic)
                        xmps)

        ;; Generalized roll-up functionality
        p100    #(* 100. (/ (stats %)
                            (stats :count)))

        zero    #(apply zero-hashmap %)
        init    #(vector (map %1 xmps)                          ; [elements, initial count-map]
                         (zero %2))
        kount   #(update-values %1 %2 inc)                      ; Accumulate hits from seq %2

        count-tokens    (fn [zeros elements]
                          (reduce kount
                                  zeros
                                  (flatten (map #(remove empty? %) elements))))

        count-texts     (fn [zeros elements]
                          (reduce #(update-values %1 (apply set/union %2) inc)
                                  zeros
                                  elements))

        ;; We'll need a sequence of affect (rule) sets for the Texts
        [aff-rules                                              ; Affect sets from Texts
         aff-zeros] (init :analysis Affect-Names)               ; Acc init: affect counts

        aff-toks    (count-tokens aff-zeros aff-rules)
        aff-texts   (count-texts  aff-zeros aff-rules)

        ;; Six Americas surveys
        [svy-hits                                               ; Keyword hits from surveys
         svy-zeros] (init :surveys (keys Surveys))              ; Acc init: survey counts

        svy-toks    (count-tokens svy-zeros svy-hits)
        svy-texts   (count-texts  svy-zeros svy-hits)

        ;; Now get a sequence of part-of-speech tags for the Texts.
        [pos-tags                                               ; POS tags for all Texts
         pos-zeros] (init :pos-tags pos/POS-Codes)              ; Acc init: POS tag counts

        pos-toks  (reduce kount pos-zeros pos-tags)
        pos-texts (reduce (fn [cnts pos]
                            (update-values cnts (into #{} pos) inc))
                          pos-zeros
                          pos-tags)]

  ;; Report the basic statistics
  (log/fmt-info "SCR~a: p[~1$%] s[~1$%] xmps~a"
                dtag (p100 :positive) (p100 :senti) stats)

  ;; Report pos/neg first, then the emotions
  (doseq [aff (conj (sort (keys (dissoc aff-texts "Positive" "Negative")))  ; ABCize emotions
                    "Negative"                                              ; Add onto head
                    "Positive")]                                            ; ..of the list
    (log/fmt-debug "Affect~a ~12a [~4d Tokens in ~4d Texts]"
                   dtag aff
                   (get aff-toks  aff)
                   (get aff-texts aff)))

  ;; Six Americas surveys
  (log/debug)
  (doseq [svy (sort (keys svy-texts))]
    (log/fmt-debug "Survey~a ~12a [~4d Tokens in ~4d Texts]"
                    dtag (name svy)
                    (get svy-toks svy)
                    (get svy-texts svy)))

  ;; Report part-of-speech tags
  (log/debug)
  (doseq [pos (sort-by pos/POS-Fragments
                      (keys pos-texts))]
    (log/fmt-debug "Speech~a ~24a [~4d Tokens in ~4d Texts]"
                   dtag
                   (pos/POS-Fragments pos)
                   (get pos-toks  pos)
                   (get pos-texts pos))))))



;;; --------------------------------------------------------------------------
(defn report-scr
  "Give positive/negative coverage and sentiment statistics for the SCR elements."
  []
  (report-examples (:examples @SCR)))



;;; --------------------------------------------------------------------------
(defn report-scr-polarity
  "Give positive/negative coverage and sentiment statistics for the SCR elements."
  ([pole]
  (report-scr-polarity (which-data) pole))


  ([dtag pole]
  (let [xmps (filter #(= pole (:polarity %))
                     (-> @SCR :examples dtag))]
    (report-examples dtag xmps))))



;;; --------------------------------------------------------------------------
(defn- create-scr-examples
  "Create examples based on part-of-speech tokens.

  Example:  #3955 '- toothache subsiding, thank god for extra strength painkillers'

  Results in this entry being added to the @SCR :examples value set under the key «DECREASE-N»:
            {:tid «t3955»
             :polarity :positive
             :pos-tags («,» «N» «V»             «,»  «V»   «^» «P» «A» «N» «N»)
             :rules    (#{} #{} #{«DECREASE-N»} #{} #{}    #{} #{} #{} #{} #{})}
             :analysis (#{} #{} #{«DECREASE-N»} #{} #{«P»} #{} #{} #{} #{} #{})}"
  [dset arff]
  ;; Create the official set of Text examples
  (log/fmt-debug "Loading dataset~a: ~a" dset arff)
  (instances->examples dset arff))



;;; --------------------------------------------------------------------------
(defn create-scr!
  "Loads the SCR examples and ontologies."
  ([]
  (create-scr! INIT-DATA-TAG))


  ([dtag]
  (create-scr! dtag (ARFFs dtag)))


  ([dtag arff]
  (create-scr! dtag arff false))


  ([dtag arff solns?]
  (let [xmps (create-scr-examples dtag arff)]

    ;; Give feedback on the examples while we're building the ontology
    (future
     (report-examples xmps))

    ;; Set our top-level state
    (reset! SCR {:examples xmps
                 :ontology (populate-scr-ontologies xmps solns?)})

    ;; Return the collection of rule tags
    (keys xmps))))



;;; --------------------------------------------------------------------------
(defn partition-scr!
  "Splits SCR examples and ontology by positive and negative individuals for
  the specified (or default) data tag."
  ([]
  (partition-scr! INIT-DATA-TAG))


  ([dtag]
  ;; Create the keys for the new partitions
  (let [dtag-p (keyize dtag :- :positive)
        dtag-n (keyize dtag :- :negative)]
    ;; Do all the work atomically
    (swap! SCR
      (fn [scr]
        (let [all-xmps  (:examples scr)
              all-onts  (:ontology scr)
              xmps      (all-xmps dtag)
              ont       (all-onts dtag)]
          ;; Do we know this data tag?
          (if (and xmps
                   ont)
            ;; Find the positive texts and use them to partition the examples
            (let [ptexts (learned-positive-texts ont)
                  xparts (group-by #(if (contains? ptexts (individual ont (:tid %)))
                                        dtag-p
                                        dtag-n)
                                   xmps)]
              ;; NOTE: the added p/n ontologies will not have any learned solutions
              {:examples (merge all-xmps xparts)
               :ontology (merge all-onts (populate-scr-ontologies xparts))})

            ;; Unknown data tag...no change!
            scr))))


    (report-scr))))



;;; --------------------------------------------------------------------------
(defonce ^:private Base-Instances (weka/load-arff (:base ARFFs) (which-target)))

(defn- ^Instances base-data
  "Returns the base say-senti data evaluation Instances"
  []
  Base-Instances)


(defn ^Instances rebase-data
  "Creates a new set of Weka Instances with the say-senti base structure.
  Specify a tag to append it to the Instances' relation name."
  ([]
  (Instances. ^Instances Base-Instances 0))


  ([tag]
  (rebase-data tag Base-Instances))


  ([tag base]
  (let [insts (Instances. ^Instances base 0)
        rname (str (.relationName ^Instances base)
                   (name tag))]
    (.setRelationName ^Instances insts rname)
    insts)))



;;; --------------------------------------------------------------------------
(defn rebase-data->hashmap
  "Creates a hashmap with the specified keys where every value is a Weka
  Instances object the say-senti base structure."
  ([tags]
  (rebase-data->hashmap tags Base-Instances))


  ([tags data]
  (into {} (map #(vector % (rebase-data % data))
                tags))))



;;; --------------------------------------------------------------------------
(defn- extend-data
  "Adds part-of-speech counts as additional attributes to a set of Instances
  with POS-tagged text as the specified zero-based attribute (tndx).  The Weka
  function returns the updated Instances as a convenience."
  [^Instances insts
   ^Integer   tndx]
  (let [rpos  (set/map-invert pos/POS-Fragments)                ; Reversed POS {name -> code}
        zeros (into {} (map #(vector % 0)
                             (vals rpos)))
        attrs (loop [acnt  (.numAttributes insts)               ; Attribute lookup {code -> index}
                     names (sort (keys rpos))
                     attrs {}]
                ;; Create a mapping of POS code to counter Attribute
                (if (empty? names)
                    attrs
                    (let [aname (first names)
                          attr  (Attribute. aname)]
                      (.insertAttributeAt insts attr acnt)
                      (recur (inc acnt) (rest names) (assoc attrs (get rpos aname) acnt)))))]

    (doseq [^Instance inst (weka/instance-seq insts)]
      ;; Get counts for each POS in this Instance's text
      (let [text   (.stringValue inst tndx)
            codes  (map #(first (str/split % #"_" 2))           ;[2] Pull code: ("A"    "P"    "D"    "N"   "N")
                                (str/split text #" "))          ;[1] Break up :  "A_sad  P_for  D_my   N_APL N_friend"
            counts (reduce #(update %1 %2 inc) zeros codes)]    ;[3] POS count: {"A" 1, "P" 1, "D" 1, "N" 2, ... zeros}
        ;; Update each Attribute for this instance with the associated count
        (run!
          (fn [[code cnt]]
            (.setValue inst (int (get attrs code))
                            (double cnt)))
          counts)))

    ;; Return the dataset as a convenience
    insts))



;;; --------------------------------------------------------------------------
(defn- ^Instance init-instance
  "Creates an instance for a sentiment analysis ARFF dataset, setting the
  first three attributes (id, text, sentiment).  The new Instance is set as
  belonging to the specified dataset, but the function does *NOT* add it to
  that dataset."
  ([^Instances  oinsts
    ^Instance   iinst]
  ;; There may be more, but we're just handling the three always-present attributes
  (init-instance oinsts (.stringValue iinst (int (Columns-Film :id)))
                        (.stringValue iinst (int (Columns-Film :text)))
                        (.value       iinst (int (Columns-Film :sentiment)))))


  ([^Instances  oinsts
    ^String     id
    ^String     text
    ^Double     sentiment]
  ;; Create the new Instance and link (but don't add) it to the dataset
  (doto (DenseInstance. (.numAttributes oinsts))
        (.setDataset oinsts)
        (.setValue (int (Columns-Film :id)) id)
        (.setValue (int (Columns-Film :text)) text)
        (.setValue (int (Columns-Film :setiment)) sentiment)))) ; 0.0=neg, 1.0=pos



;;; --------------------------------------------------------------------------
(defn add-instance
  "Make a instance for a sentiment analysis ARFF dataset.  The first three
  attributes must match those of the base dataset (id, text, sentiment)."
  ([^Instances  oinsts
    ^Instance   iinst]
  ;; Start with the base three attributes...
  (let [oinst (init-instance oinsts iinst)]
    ;; Copy the remaining attributes
    (doseq [^Long i (range (inc (Columns-Film :sentiment))
                           (.numAttributes oinsts))]
      (.setValue oinst i (.value iinst i)))
    (.add oinsts oinst)
    oinst))


  ([^Instances  oinsts
    ^String     id
    ^String     text
    ^Double     sentiment]
  ;; We're just handling the three always-present attributes
  (let [oinst (init-instance oinsts id text sentiment)]
    (.add oinsts oinst)
    oinst)))



;;; --------------------------------------------------------------------------
(defn affective-instances?
  "A simple check to ensure that a set of data instances begins with the
  attributes <id, text, sentiment> and has additional attributes that are
  assumed to keep affective values for a text."
  [^Instances insts]
  (let [aname #(.name (.attribute insts (int %)))
        attrs ["id", "text", "sentiment"]
        acnt  (count attrs)]
    (boolean
     (and (> (.numAttributes insts) acnt)
          (every? #(= (attrs %)
                      (aname %))
                  (range acnt))))))



;;; --------------------------------------------------------------------------
(defn stoic-instance?
  "Determines if the given instance has zero values for all affective attributes
  Note that the function simply ignores the first three attributes, assuming the
  full dataset has previously been checked with `affective_instances?`.  The
  subsequent attributes specify the affect information for the instance"
  ([inst]
  (stoic-instance? true inst))


  ([skip-neutrals? ^Instance inst]
  (boolean
   (when skip-neutrals?
     (let [acnt (.numAttributes inst)
           vs   (.toDoubleArray inst)       ; Weka's internal data storage
           aff? #(pos? (aget vs %)) ]       ; Non-zero value for affect attribute
       (loop [ai (inc (.classIndex inst))]  ; Affect data comes after the class
         (cond
          (>= ai acnt)  true
          (aff? ai)     false
          :else         (recur (inc ai)))))))))



;;; --------------------------------------------------------------------------
(defn ^Instances load-arff!
  "Loads the dataset specified by arff into the atom dset if it has not
  previously been loaded.  The caller may request the «COMPLETE» version
  of the dataset using a truthy value for all-data?."
  ([dset arff]
  (load-arff! dset arff false))


  ([dset arff all-data?]
  (swap! dset
         #(if % ; Got a dataset?
              % ; That's the data!
              (weka/load-arff (if all-data?
                                  (weka/tag-filename arff  "COMPLETE")  ; Potentially a huge dataset
                                  arff)                                 ; Probably a less huge subset
                              (which-target))))))




;;; --------------------------------------------------------------------------
(defn create-arffs
  "Converts a multi-source input CSV (usually, DATASET) to a separate
  ARFF for each source."
  [& args]
  (let [[fpath
         opts]  (optionize string? DATASET args)                ; Optional CSV must be first arg
        suffix  (option-str [:test :weka] opts ".")             ; Information tag for output
        sconf   (cfg/? :senti)
        txt-ndx (get sconf :text-index INIT-TEXT-INDEX)         ; 1-based index
        lex-tag (get sconf :lexicon INIT-LEX-TAG)
        acnt    (.numAttributes (base-data))
        dsets   (atom {})                                       ; Collects datasets from CSV

        emoter  #(tw/make-lexicon-filter lex-tag txt-ndx)
        tagger  #(tw/make-tagging-filter txt-ndx)

        xform   (fn [iinsts]
                  ;; Call to make a new Filter each time
                  (reduce
                    #(weka/filter-instances %1 (%2))
                    iinsts
                    [emoter tagger]))

        line-up (fn [rdr]
                  (let [[_ & dlines] (csv/read-csv rdr)]        ; Skip CSV header
                    ;; The configuration may want to use a subset of the CSV data
                    (if-let [icnt (get sconf :num-instances)]
                      (take icnt dlines)
                      dlines)))

        dset+   (fn [src]
                  ; Make new dataset
                  (let [insts (rebase-data src)]
                    (log/fmt-info "Adding dataset ~a~a" src suffix)
                    (swap! dsets assoc src insts)
                    insts))

        data+   (fn [[id pn src raw]]
                  ;; Add data Instance; remember, the Weka objects are mutable
                  (let [text  (str/trim raw)
                        insts (if-let [ds (get @dsets src)] ds (dset+ src))]
                    ;(log/fmt-debug "~a<~a> [~a] ~a" src id (label-polarity pn) text)
                    (add-instance insts id text (Float/parseFloat pn))))]   ; 0.0=neg, 1.0=pos

    ;; Process the CSV, separating the sentiment sources
    (try
      (with-open [rdr (io/reader fpath)]
        (loop [dlines (line-up rdr)]
          (when-let [line (first dlines)]
            (data+ line)
             (recur (rest dlines)))))

      ;; Save the ARFFs to disk and return the result info in a map
      (reduce (fn [acc [dset iinsts]]
                (let [oinsts (xform iinsts)                 ; Process lexicon & tag tokens w/ POS
                      otag   (str dset suffix)]             ; Output ARFF filename tag(s)
                  ;; Finally, save the processed datasets from the CSV as ARFF
                  (assoc acc (keyword dset)
                             {:count (.numInstances ^Instances oinsts)
                              :fpath (weka/save-file fpath otag oinsts :arff)})))
              {}
              @dsets)

      (catch Exception ex
        ;; Oh no!  It's probably a bad character in a tweet in the CSV
        (log/fail ex (str "Problem near tweet #"
                         (reduce (fn [acc [_ dset]] (+ acc (.numInstances ^Instances dset))) 1 @dsets)))))))



;;; --------------------------------------------------------------------------
(defn split-data
  "Splits up an input ARFF into chunks we can use for DL-Learner and Weka.
  The arity 0 and 1 clauses respectively build for the default dataset and
  the default number of subsets.  The arity 2 clause does the setup and then
  makes multiple calls the worker (arity 6) clause (which you normally won't
  want to call directly).

  FIXME: This function uses the data tag (dtag) in an ambiguous manner.
         It either identifies the dataset of it's a flag that this is a
         weka dataset. Our ambiguity-bridge is (ARFFs :weka)."
  ([]
  (split-data INIT-DATA-TAG))


  ([dtag]
  (split-data dtag (cfg/? :senti)))


  ([dtag {:keys  [data-split]
          :as    sconf
          :or    {data-split INIT-DATA-SPLIT}}]
  ;; Pull what we need from the config before creating the cnt datasets
  (let [{:keys  [datasets parts train rand-seed]} data-split
        dset    (atom nil)                                      ; Load input ARFF only if needed
        target  (inc (.classIndex (base-data)))                 ; 1-based dependent attribute index
        reattr  ["-R" (str (inc target) "-last," target)]]      ; Reorder filter opts: "4-last,3"

    ;; Sample (semi)full ARFF to create cnt train/test dataset pairs
    (into {}
      (domap
        (fn [n]
          (let [rseed    (+ rand-seed n)
                trn-tst  (split-data dtag dset rseed reattr sconf)
                arffs    (if (= dtag :weka)
                             ;; That's all the Weka Experimenter needs
                             trn-tst
                             ;; Chop trainers into parts for DL-Learner
                             (let [ftrain (:train trn-tst)
                                   dtrain (atom nil)                ; Only load trainers if needed
                                   subcnt (quot train parts)        ; Number of instances in a part
                                   extras (rem  train parts)]       ; Leftovers from an uneven split
                               ;; Build a sequence of ARFF fpaths for the training sub-splits
                               (assoc trn-tst
                                      :parts
                                      (domap #(let [fsub (enumerate-dataset ftrain %)]  ; Suffix: part num
                                                ;; Only rebuild if necessary
                                                (if (.exists (io/file fsub))
                                                  fsub
                                                  (let [iinsts (load-arff! dtrain ftrain)   ; Load trainers once
                                                        oinsts (Instances. iinsts           ; Instances subset
                                                                           (int (* % subcnt))
                                                                           (int subcnt))]

                                                    (log/fmt-info "Creating sub-split ~a: cnt[~a] all-affect[~a]"
                                                                  % subcnt (yn (affective-instances? iinsts)))

                                                    (when-not (zero? extras)
                                                      (log/warn "Extra instances:" extras))

                                                    (weka/save-file fsub oinsts))))
                                             (range parts)))))]

            ;; We are building a hashmap
            [(key-prng rseed) arffs]))

        (range datasets)))))


  ([dtag dset seed reattr {:keys [all-data? skip-neutrals? text-index]
                           :or   {text-index INIT-TEXT-INDEX}}]
  ;; This is the workhorse clause.  It is not meant to be called directly
  (let [rtag    (str "r" seed)
        ipath   (ARFFs dtag)
        trn-tst (into {} (map #(let [ttag (str rtag "." (name %))]              ; pRNG seed & train|test tag
                                 [% (weka/tag-filename ipath ttag :arff)])      ; Weka split sub-dataset
                              SPLIT-TAGS))]
    ;; Do the train/test ARFFs already exist?
    (if (every? #(.exists (io/file %)) (vals trn-tst))
      trn-tst
      (let [goals   (split-pn-goals dtag)
            iinsts  (load-arff! dset ipath all-data?)
            icnt    (.numInstances iinsts)

            dsets   (atom (rebase-data->hashmap SPLIT-TAGS iinsts))
            rng     (Random. seed)
            fill    (fn [used [^Instances oinsts cnts goal
                               :as data-info]]
                      (if (creation-done? cnts goal)
                        ;; We're done filling the dataset.  The used-index set is the accumulator.
                        used
                        ;; Keep pulling from the input data
                        (let [ndx (.nextInt rng icnt)]
                          (if (contains? used ndx)
                              (do ;(log/debug "Resampling on repeat index:" ndx)
                                  (recur used data-info))
                              (let [inst  (.get iinsts ndx)
                                    pn    (polarize inst)
                                    used* (conj used ndx)]
                                (if (or (creation-full? cnts pn goal)           ; Finished one of the buckets?
                                        (stoic-instance? skip-neutrals? inst))  ; Ignore non-affective data?
                                  ;; Add this index to the "used" set, but don't add instance
                                  (recur used* data-info)
                                  (do ;(println "Adding to" (.relationName oinsts) "#" ndx)
                                      (add-instance oinsts inst)
                                      (recur used* [oinsts
                                                    (inc-pn-counter cnts dtag pn)
                                                    goal]))))))))

            wfilter (fn [data tt]
                      ;; Finalize the dataset for Weka
                      (let [reorder (Reorder.)
                            insts*  (-> (data tt)
                                        (extend-data (weka/index1->0 text-index))   ; Insert POS counts
                                        (weka/filter-instances reorder reattr))]    ; Remove text & put class last
                        (assoc data tt insts*)))]

        ;; Fill up the datasets with random instances from the input set
        (reduce fill
                #{}                                                 ; Set of used indices
                (map (fn [[tt insts]]                               ; Instances & counts for train/test
                        (log/info (describe-creation goals tt)
                                  "instances:" rtag)
                        [insts (zero-pn-counter dtag) (goals tt)])
                     @dsets))

        ;; If these are Weka datasets, prepare them for the Experimenter.
        ;; Note, this updates the dsets atom with new Instances.
        (when (= dtag :weka)
          (log/debug "Filtering datasets for Weka")
          (run! #(swap! dsets wfilter %) SPLIT-TAGS))

        ;; Save the output train/test datasets & return a map of the ARFF paths
        (into {} (domap (fn [[tt ^Instances insts]]
                          (.randomize insts rng)                    ; Distribute pos/neg more-or-less evenly
                          [tt (weka/save-file (trn-tst tt) insts)]) ; ARFF fpath w/ pRNG seed & train|test tag
                        @dsets)))))))



;;; --------------------------------------------------------------------------
(defn get-split-arffs
  "Retrieves a sequence of ARFF filepath from a split-data result map."
  ([arffs]
  (get-split-arffs arffs (cfg/? :senti)))


  ([arffs {:keys [data-split]}]
  ;; TODO: We currently process only data for the first (configured) random seed,
  ;;       but we will need to handle a series of datasets starting from that seed.
  (let [{:keys [iterate?
                rand-seed]} data-split
        rpng  (key-prng rand-seed)
        split (arffs rpng)]

    (log/fmt-debug "Retreiving data split ~a" (name rpng))
    (if iterate?
        (:parts split)
        (list (:train split))))))



;;; --------------------------------------------------------------------------
(defn pn-examples
  "Returns a map of the IDs of tweets with positive polarities and those with
  negative polarities.  The caller may optionally specify a prefix for easy
  insertion into a DL-Learner configuration file."
  ([rule]
  (pn-examples rule ONT-PREFIX))

  ([rule prefix]
  (pn-examples rule prefix (get-in @SCR [:examples rule])))

  ([rule prefix examples]
  (let [tag     (str prefix (when prefix ":"))
        tagger  #(str tag %)
        tids    (reduce (fn [acc {:keys[tid polarity]}]
                          (update-in acc [polarity]
                                         #(conj % tid)))
                        {:positive (sorted-set)             ; Collect IDs for pos/neg examples
                         :negative (sorted-set)}
                        examples)
        xmps    (update-values tids #(map tagger %))]       ; Prefix % tag pos/neg IDs

    ;; Save P/N Text to pull in for DL-Learner runs
    (spit (str "resources/emo-sa/pn-examples-" (name rule) ".edn")
          (pr-str xmps))

    xmps)))



;;; --------------------------------------------------------------------------
(def ^:dynamic *reason-log-limit* 2000)         ; Rebind for more|less logging

(defn reason-log-limit
  "Returns the number of log lines that reasoning operations should generate,
  based on the specified options.  Currently :no-log is the only supported
  option."
  [& opts]
  (if (some #{:no-log} opts)
      0
      *reason-log-limit*))



;;; --------------------------------------------------------------------------
(defn make-monitor
  "Creates a custom monitor for a HemiT reasoner."
  [& opts]
  ;; FIXME: Move this to say.ontology when stable
  (let [tableau   (atom nil)
        start-ms  (atom 0)
        forms     (agent {})
        log-limit (agent *reason-log-limit*)
        log!?     #(and (send log-limit dec)
                        (pos? @log-limit))
        log       #(when (log!?) (apply println %&))
        log-val   #(when (log!?) (pprint %))
        prefixes  (reduce
                    (fn [^Prefixes ps [tag iri]]
                      (.declarePrefix ps tag iri)
                      ps)
                    (Prefixes.)
                    (merge (update-keys PREFIXES #(str % ":")) ; The OWL API wants the colon
                           Prefixes/s_semanticWebPrefixes))]

    ;; The local logger uses println, so sync first with the main logger
    (await log/Logger)

    ;; TODO: If we proixy TableauMonitorAdapter (rather than CountingMonitor),
    ;;       then remove the proxy-super calls to clean up the reflection warnings.
    (proxy [TableauMonitorAdapter] []

      (setTableau [tbl]
        ;; Our parent class keeps a protected copy.  Use that if we convert to gen-class
        (log "Tableau:" (str tbl))
        (reset! tableau tbl))

      (isSatisfiableStarted [task]
        (let [descr (str task)]
          (log "Checking" descr)
          ;; We need to check the timing for ABox satisfiability (consistency)
          (when (str/starts-with? descr "ABox")
            (reset! start-ms (System/currentTimeMillis)))))

      (isSatisfiableFinished [task result]
        (log (if result "YES" "NO"))
        ;; Turn off logger after ABox!
        (when (str/starts-with? (str task) "ABox")
          (send log-limit (fn [_] 0))
          (await forms)
          (let [fcnts @forms]
            (log "ABox satisfiability checked in" (- (System/currentTimeMillis) @start-ms) "ms")
            (log "Checks on individuals:")
            (log-val fcnts)
            (log "TOTAL:" (reduce + 0 (vals fcnts))))))

      (saturateStarted []
        (comment (log "Saturate started")))

      (nodeCreated [node]
        (comment (log "Node:" (str node "/" (.getTreeDepth node)))))

      (addFactStarted [tuple core?]
        (comment
          (log "Fact:" (when core? "[core]") (count tuple))
          (doseq [elm (seq tuple)]
            (log "-" (str elm)))))

      (dlClauseMatchedStarted [clause ndx]
        (comment (log (str "DL clause<"  ndx ">\n" clause))))

      (startNextBranchingPointStarted [^BranchingPoint branch]
        ; NOTE: This method is called for our POS punned individuals, but not the Text & Tokens
        (comment (log "Branch point<" (.getLevel branch) ">")))

      (pushBranchingPointStarted [^BranchingPoint branch]
        (when-let [^Tableau tbl (and branch @tableau)]
          (when-let [dsj (.getFirstUnprocessedGroundDisjunction tbl)]
            (let [form      (.toString dsj prefixes)
                  [lvar _]  (str/split form #"\(" 2)]           ; Strip the node ID from the logical var
              ;; First time? log the var as a sample!
              (when-not (get @forms lvar)
                (log (log/<> "bp" (.getLevel branch)) form))
              ;; Keep counts for logical vars
              (send forms
                    #(let [cnt (get % lvar 0)]                  ; How many times have we seen this variable?
                       (assoc % lvar (inc cnt))))))))

      (backtrackToFinished [branch]
        (comment (log "Backtracking<" (.getLevel branch) ">"))))))



;;; --------------------------------------------------------------------------
(defprotocol Reasoning
  "Look into exactly what is going on during reasoning."
  ;; TODO: Move this code to say.ontology once it's stable

  (invoke-reasoner [rsnr ont]
    "Returns the reasoner (not the factory) connected to Tawny-OWL for the specified ontogy.")

  (^Reasoner make-reasoner [rsnr ont]
    "Create an OWL reasoner outside of Tawny-OWL so we can customize it.")

  (show-reasoning [rsnr ont]
    "Look into exactly what is going on during reasoning."))


(extend-protocol Reasoning
  clojure.lang.Keyword
  (invoke-reasoner [rsnr ont]
    (if (rsn/reasoner-factory rsnr)
        (rsn/reasoner ont)
        (log/error "Reasoner" (name rsnr) "is not supported in Tawny OWL!")))


  (make-reasoner [rsnr ont]
    (let [cfg (Configuration.)
          mon (make-monitor)]
      ;; If we set the monitor type, it's an additional monitor to the custom one we're attaching
      ;(set! (.-tableauMonitorType cfg) Configuration$TableauMonitorType/TIMING)
      (set! (.-monitor cfg) mon)
      (Reasoner. cfg ont)))


  (show-reasoning [rsnr ont]
    (show-reasoning (make-reasoner rsnr ont) ont))


  org.semanticweb.HermiT.Reasoner
  (show-reasoning [rsnr ont]
    (log/info "Reasoner:" (type rsnr))
    (.precomputeInferences rsnr (into-array [InferenceType/CLASS_HIERARCHY]))
    rsnr))

    ;(binding [rsn/*reasoner-progress-monitor* (atom rsn/reasoner-progress-monitor-text-debug)]
    ;  (rsn/consistent? ont))))



;;; --------------------------------------------------------------------------
(defn reason
  "Runs the specified OWL reasoner on (a) say-senti ontology."
  ([]
  (reason :hermit))

  ([rkey]
  (reason rkey say-senti))

  ([rkey ont & opts]
  (binding [*reason-log-limit*  (apply reason-log-limit opts)]
    (show-reasoning rkey ont))))



;;; --------------------------------------------------------------------------
(defn run-timings
  "Performs timing checks for an ontology (defaults to :Sentiment140)."
  ([]
  (run-timings INIT-DATA-TAG))

  ([tag]
  (if-let[ont (get-in @SCR [:ontology tag])]

    ;; Right now, we're just testing with HermiT.
    (let [rsnr (make-reasoner :hermit ont)]
      (run! #(do (print (str %) ":")
                 (time (.precomputeInferences rsnr (into-array [%]))))
           Inferences))

    ;; Oops, someone skipped a step!
    (println "Please execute 'run' to create the ontologies"))))



;;; --------------------------------------------------------------------------
(defonce Soln-With (into Affect-Names pos/POS-Names))   ; Set of desired elements

(defn good-solution?
  "Returns  passed a 'useful' solution string (as repoted by DL-Learner)"
  [soln]
  ;; First make sure there are no silly-solution markers
  (when-not (re-find Soln-Without soln)
    ;; Split it into basic elements and make sure there's at least one we want
    (not (empty? (filter Soln-With
                         (remove empty? (str/split soln #" |\(|\)")))))))



;;; --------------------------------------------------------------------------
(defn process-solutions
  "Handles candidate solutions.  This function is still in FLUX."
  [solns]
  (let [learn-cap (cfg/?? :senti :learn-cap INIT-LEARN-CAP)]

    ;; Log the full solution set to make sure the ordering is correct
    (doseq [soln solns]
      (log/fmt-debug "SOLN: ~a" soln))

    ;; Cap the solutions we keep from those with the elements we want and without those we don't
    (with-open [ss (io/writer Soln-Log :append true)]
      (.write ss (str ";;; -" (java.util.Date.)
                      "-------------------------------------------\n"))
      (doseq [soln (take learn-cap
                         (filter good-solution? solns))]
        (.write ss (str soln "\n"))))
    (log/info "Solutions saved to" Soln-Log)))



;;; --------------------------------------------------------------------------
(defn run
  "Runs a DL-Learner session to determine equivalent classes for Positive Texts.
  Supported options are:
    :arff       Create Weka datasets from original CSV data
    :reset      Deletes the solutions file before commencing learning
    :learn      Use rules learned in previous iterations as learning continues
    :timings    Run timed tests with HermiT on each iteration
    :weka       No ontological learning; just prepare data for Weka explorer"
  [& opts]
  ;; Shall we do some precleaning
  (when (some #{:reset} opts)
    (log/info "Recreating solutions file:" Soln-Log)
    (io/delete-file Soln-Log true))

  ;; Recreate our source ARFF if num-examples has been updated in the Config
  (when (some #{:arff} opts)
    (apply create-arffs opts))

  ;; Will this be Sentiment140 or another dataset?
  (let [base    "say-senti"
        dtag    (apply which-data opts)
        sconf   (cfg/? :senti)
        dpaths  (split-data dtag sconf)

        check!  (fn [arff]
                  (log/debug "ARFF:" arff)
                  ;; (Re)generate the examples and ontologies from the ARFF
                  (create-scr! dtag arff (some #{:learn} opts))
                  (save-ontologies)

                  ;; Do reasoner tests if requested
                  (when (some #{:timings} opts)
                    (run-timings))

                  ;; Run batch with DL-Learner
                  (update-kv-values (:examples @SCR)
                    (fn [rule xmps]
                      (dll/write-pn-config :base     base
                                           :rule     rule
                                           :prefixes (merge PREFIXES {"scr" (ont-iri rule)})
                                           :examples (pn-examples rule "scr" xmps))))
                  (process-solutions (dll/run base dtag)))]

    ;; For DL-Learner (non-weka), run the first of the data (sub)splits
    (if (some #{:weka} opts)
        (log/notice "Created" (count dpaths) "train/test ARFF pairs")   ; No ontological run for Weka
        (run! check! (get-split-arffs dpaths sconf)))))                 ; TODO: Run all splits


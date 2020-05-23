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
            [say.dllearner      :as dll]
            [say.dolce          :as dul]
            [say.log            :as log]
            [say.cmu-pos        :as pos]
            [weka.core          :as weka]
            [weka.tweet         :as tw]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp pprint]]
            [defun.core         :refer [defun]]
            [tawny.english      :as dl]
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

(def ^:const ONT-ISTUB  "http://www.dendrown.net/uqam/say-senti")
(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-senti.owl#")
(def ^:const ONT-FSTUB  "resources/KB/say-senti")
(def ^:const ONT-FPATH  "resources/KB/say-senti.owl")
(def ^:const ONT-PREFIX "senti")
(def ^:const DATASET    "resources/emo-sa/sentiment-analysis.csv")
(def ^:const COUNTS     {:Sentiment140  1577278
                         :Kaggle        1349})
(def ^:const ARFFs      {:base          "resources/emo-sa/sentiment-analysis.arff"
                         :Kaggle        "resources/emo-sa/sentiment-analysis.Kaggle.arff"
                         :Sentiment140  "resources/emo-sa/sentiment-analysis.Sentiment140.arff"
                         :weka          "resources/emo-sa/sentiment-analysis.Sentiment140.weka.arff"})
(def ^:const COL-ID     0)
(def ^:const COL-TEXT   1)

(def ^:const PREFIXES   {"senti"    ONT-IRI
                         "pos"      pos/ONT-IRI})

(def ^:const SPLIT-TAGS [:train :test])
(def ^:const TWEET-TAG  "t")                        ; Tweet individual have this tag plus the ID ( "t42" )


;;; --------------------------------------------------------------------------
;;; TODO: Automate solution handling
(def ^:const SOLN-LOG       "resources/emo-sa/say-senti.solutions.edn")
(def ^:const SOLN-WITH      #"\bdenotesAffect\b|\bisPartOfSpeech\b")
(def ^:const SOLN-WITHOUT   #"\bThing\b")
(def ^:const LEARNED-POS    "LearnedPositiveText")  ; Equivalency class from DL-Learner results


;;; --------------------------------------------------------------------------
;;; Default values for configuration elements
(def ^:const INIT-NUM-EXAMPLES  100)
(def ^:const INIT-DATA-TAG      :Sentiment140)
(def ^:const INIT-DATA-SPLIT    {:datasets 10, :train 500, :test 500, :parts 10 :rand-seed 1})
(def ^:const INIT-LEX-TAG       :nrc)
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


;;; --------------------------------------------------------------------------
(defonce SCR-Examples   (atom {}))
(defonce SCR-Ontologies (atom {}))

(defonce Expressions    (if (cfg/?? :senti :use-scr?)
                            ;; Word sets which invoke Sentiment Composition Rules
                            {"DECREASE-N"   #{"alleviate" "avoid" "handle" "lessen" "mitigate" "relieve"
                                              "resolve" "soothe" "subside" "waive"}

                             "DECREASE-P"   #{"lack" "lose" "omit" "miss"}
                             "INCREASE-N"   #{"burst" "climb" "escalate" "intensify"
                                             ; 2-grams: "go up" "grow" "mark up" "pile up"
                                             }
                             "INCREASE-P"   #{"elevate" "enlarge" "expand" "extend" "increase" "progress"
                                              "raise" "return" "rise" "soar" "surge"
                                             ; 2-grams: "be up" "build up" "come back"
                                             }}
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

(if false
    ;; FIXME: Decide how we're handling the InfoObj subclass(es)
    (do
      (as-subclasses dul/InformationObject
      :disjoint
      (defclass Text
        :label   "Text"
        :comment "An Information Object consisting of text.")

      ; TODO: Differentiate between Punctuation as an Information Object and a "Part of Speech" Quality
      ;(defclass Punctuation
      ;  :label   "Punctuation"
      ;  :comment (str "An Information Object representing a grammatical symbol to organize and"
      ;                "aid the understanding of written text."))

      (defclass Term
      ;TODO:  Consider splitting off: numeral, emoticon, hashtag, @mention
        :label   "Term"
        :comment "An Information Object representing a syntactic unit of meaning, such as a word."))

      (refine Term :equivalent pos/Token))      ; cmp: (refine pos/Token :equivalent (dl/or Term Punctuation))

  ;; FIXME: Of the three potential InfoObj classes, Text is the only one we actually use
  (defclass Text
    :super   dul/InformationObject
    :label   "Text"
    :comment "An Information Object consisting of text."))

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
  (defscr DECREASE-N  "Expressions which decrease NPI and NE terms.")
  (defscr DECREASE-P  "Expressions which decrease PPI and PO terms.")
  (defscr INCREASE-N  "Expressions which increase NPI and NE terms.")
  (defscr INCREASE-P  "Expressions which increase PPI and PO terms."))


;;; --------------------------------------------------------------------------
(defclass Affect
  :super    dul/Concept
  :label    "Affect"
  :comment  "A concept describing human sentiment or emotion.")

(defoproperty denotesAffect
  :super    dul/expresses
  :label    "denotes affect"
  :domain   pos/Token
  :range    Affect
  :comment  "A relationship between a Text and the affect it expresses.")

;;; TODO: Resolve the discrepancy between pos/neg sentiment Affect and the P/N SCRs
;;;       Also, these affect concepts should use noun forms (Positivity and Negativity),
;;;       but we're using terms from the lexicon for the initial evaluation.
(as-subclasses Affect
  (defclass Positive
    :label   "Positive"
    :comment "An affective concept representing positive sentiment polarity.")

  (defclass Negative
    :label   "Negative"
    :comment "An affective concept representing negative sentiment polarity."))

(run! #(defpun %) [Positive Negative])


(defmacro defemotion
  "Adds a Concept reprenting an emotion to the say-senti ontology"
  [emo sys]
  `(do
     (defclass ~emo
       :super   Affect
       :label   (name '~emo)
       :comment (str "A concept which expresses the class of human affect generally known as "
                     (str/lower-case (name '~emo))
                     (when ~sys
                       (str " according to the system of base emotions by " (str/capitalize (name ~sys))))
                     "."))
       (defpun ~emo)))


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
(defemotions (cfg/?? :senti :emotions))

(rsn/reasoner-factory :hermit)
(defonce Affect-Fragments   (into {} (map #(let [a (iri-fragment %)]
                                             [(lower-keyword a) a])
                                           (rsn/instances Affect))))
(defonce Affect-Names       (into #{} (vals Affect-Fragments)))

;;; We must declare the different types of Aspect to be disjoint for the reasoner
;;; to handle equivalency classes based on the complement of a given Aspect.
(apply as-subclasses Affect :disjoint (map #(owl-class %) Affect-Names))

;;; --------------------------------------------------------------------------
;;; TODO: Put SentimentPolarity back in after we handle timing considerations
;;;       Make sure it's moved above the HermiT Reasoner invocation.
;(defclass SentimentPolarity
;  :super   dul/Quality
;  :label   "Sentiment Polarity"
;  :comment "A Quality describing an Information Object's expression as positive, negative, or neutral.")
;
;(as-subclasses SentimentPolarity
;  :disjoint
;  (defclass PositiveSentimentPolarity
;    :label   "Positive Sentiment Polarity"
;    :comment "A Sentiment Polarity expressing positive sentiment")
;  (defclass NegativeSentimentPolarity
;    :label   "Negative Sentiment Polarity"
;    :comment "A Sentiment Polarity expressing negative sentiment"))
;(defpun PositiveSentimentPolarity)
;(defpun NegativeSentimentPolarity)
;
;(defoproperty hasPolarity
;  :super    dul/hasQuality
;  :label    "has Polarity"
;  :domain   dul/InformationObject
;  :range    SentimentPolarity)

;;; Tell DL-Learner about our ontology elements
(dll/register-ns)

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
(defn make-iri
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
                 :iri     (make-iri scr)
                 :prefix  (prefix "scr")
                 :import  say-senti
                 :comment (str "Ontology for training sentiment models wrt. the Sentiment Composition Rule " scr))

        ;; Create a parent class for output representations as described by DL-Learner
        dltext (owl-class ont LEARNED-POS
                 :super    Text
                 :label    "Learned Positive Text"
                 :comment  (str "A Text representing a candidate formula for determining if a given Text "
                                "expresses positive sentiment."))

        add-dl (fn [n expr]
                 (owl-class ont (str LEARNED-POS "-" n)
                   :super dltext
                   :equivalent (eval expr))
                 (inc n))]

    ;; If we've got learned expressions, create corresponding classes to describe Postive Texts
    (when learned
      (reduce add-dl 1 learned))

    ont)))



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
    (try
      (polarize (.classValue inst))
      (catch weka.core.UnassignedClassException _ :?)))


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
    (label-text (.value inst COL-ID)))

  Object
  (label-text [x]
    (str TWEET-TAG (longify x))))



;;; --------------------------------------------------------------------------
(defn- add-text
  "Adds a Text individual to the specified Sentiment Component Rule ontology.
   A positivity clue (an OWL individual) may be specified to add an explicit
   relation:

        <Text denotesAffect PositiveToken>

   for the purpose of guiding learning or evaluating the system."
  ([ont tinfo sconf]
  (add-text ont nil tinfo sconf))


  ([ont clue
    {:keys [content id polarity pos-tags rules]}    ; Text (tweet) breakdown
    {:keys [full-links? pos-neg?]}]                 ; Senti-configuration
  ;; The code will assume there's at least one token, so make sure!
  (when (seq pos-tags)
    (let [tid     (label-text id)
          text    (individual ont tid       ; Entity representing the text
                    :type (if pos-neg?
                              (case polarity :negative NegativeText
                                             :positive PositiveText)
                              Text)
                    :annotation (annotation TextualContent
                                            (apply str (interpose " " content))))
          express #(refine ont %1 :fact (is denotesAffect (individual say-senti %2)))]

      ;; And entities for each of the terms, linking them together and to the text
      (reduce
        (fn [[cnt tokens :as info]
             [tag rules]]
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

              ;; Add positivity clue if we're going to guide learning
              (when (and clue                                   ; Optional (caller decides on clues)
                         (= cnt 1)                              ; Add the relation on the first word
                         (= polarity :positive))
                ;; Coax DL-Learner into looking at what denotes affect
                (refine ont curr :fact (is denotesAffect clue)))

            ;; Link tokens to each other
            (when-let [prev (first tokens)]
              (refine ont curr :fact (is dul/directlyFollows prev))

              ;; The reasoner can figure out the rest, but being explicit may be faster
              (when full-links?
                ;; The current Token comes after all the tokens we've seen so far
                (refine ont prev :fact (is dul/directlyPrecedes curr))
                (run! (fn [tok]
                        (refine ont curr :fact (is dul/follows tok))
                        (refine ont tok  :fact (is dul/precedes curr)))
                      tokens)))

            ;; Express sentiment composition rules
            (doseq [rule rules]
              ;; Report the real SCR rules (not P|N because there are too many)
              (when-not (contains? Affect-Names rule)
                (log/debug "Tweet token" ttid "expresses" rule))
              (express curr rule))

            ;; Continue the reduction
            [(inc cnt)
             (conj tokens curr)])

            ;; Ignored/invalid Part of Speech tag
            (do ;(log/fmt-debug "Ignoring POS tag '~a'" tag)
                info)))

        [1 nil]                             ; Acc: Token counter, reverse seq of tokens
        (zip pos-tags rules))))))




;;; --------------------------------------------------------------------------
(defn save-scr-ontologies
  "Saves Sentiment Composition Rule ontologies in OWL format."
  []
  ;; Create ontologies for each SCR, each populated with individuals expressing the rule
  (update-kv-values
    @SCR-Ontologies
    (fn [rule ont]
      (let [fpath (str ONT-FSTUB "-" (name rule) ".owl")]
        (save-ontology ont fpath :owl)
        fpath))))



;;; --------------------------------------------------------------------------
(defn save-ontologies
  "Saves the say-senti ontology and all the SCR ontologies in OWL format."
  []
  (save-ontology say-senti ONT-FPATH :owl)
  (merge {:say-senti ONT-FPATH}
         (save-scr-ontologies)))



;;; --------------------------------------------------------------------------
(defun ^OWLOntology populate-ontology
  "Populates the senti ontology using examples from the ARFFs.  The caller
  may specify an SCR identifier (keyword or string), rather than an ontology.
  If this is the case, the function will create the ontology for that rule.
  In either case function returns the ontology, populated with the new examples.
  The called may also specify a sequence of learned expressions that will be
  included in the ontology as subclasses to LearnedPositiveText."
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
  (populate-ontology (make-ontology ont learned) xmps learned sconf))


  ([ont xmps _ sconf]
  ;; Add positivity tokens if we're guiding learning (or testing the system)
  (let [clue (when (:pos-clue? sconf)
               (let [tag   "PositivityClue"
                     clazz (owl-class ont tag
                             :super   Affect
                             :label   "Positivity Clue"
                             :comment "A Positive Text indicator for guiding learners or for system evaluation.")]
                 (individual ont tag :type clazz)))]

    (run! #(add-text ont clue % sconf) xmps)
    ont)))



;;; --------------------------------------------------------------------------
(defn populate-scr-ontologies!
  "Populates the senti ontology using examples from the ARFFs"
  []
  (let [sconf (cfg/? :senti)]               ; Freeze the configuration while we work
    ;; Create ontologies for each SCR, each populated with individuals expressing the rule
    (reset! SCR-Ontologies
            (update-kv-values @SCR-Examples #(populate-ontology %1 %2 sconf)))

    ;; Return the collection of rule tags
    (keys @SCR-Ontologies)))



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
(defn instances->examples
  "When a dataset tag is specified (dset), this function returns a hashmap
  keyed by that tag plus any SCR tags which apply to one or more instances.
  The values of this hashmap are sets of hashmaps, where each (sub)hashmap
  represents an instance in 'example' form, which is an intermediate structure
  in a Text's conversion from Weka instance to ontology individual.

  When the dataset is not specified, the function only returns the set
  of example hashmaps corresponding to the specified instances."
  ([insts]
  (:data (instances->examples :data insts)))


  ([dset ^Instances insts]
  (instances->examples dset insts (.numInstances insts)))


  ([dset ^Instances insts cnt]
  (let [;;-- Keep track of how many examples to create, as per the configured 'balance' setting
        goal    (create-pn-goal dset cnt)
        all-pn? (cfg/?? :senti :skip-neutrals?)
        stoic?  (fn [rules]                                     ; Check that we're not including neutral Texts
                  (and all-pn? (every? empty? rules)))          ; ..and that no sentiment (rule) is expressed

        ;;-- We bring in examples using Weka's Affective Tweets plugin and a Snowball stemmer
        sball   (tw/make-stemmer)
        stem    #(.stem sball %)
        exprs   (update-values Expressions                      ; Pre-stem Liu's SCR expressions
                               #(into #{} (map stem %)))

        ;;-- Functions to identify pos/neg tokens and Sentiment composition rules
        lex     (tw/make-lexicon (cfg/?? :senti :lexicon :liu)) ; TODO: Capture lex change on config update
        ->sense #(tw/analyze-token+- lex % Affect-Fragments)    ; Lexicon lookup for P/N rules
        ->scr   #(let [term (stem %)]                           ; Match terms for Sentiment Composite Rules
                   (reduce (fn [acc [scr terms]]
                             (if (contains? terms term)
                                 (conj acc scr)
                                 acc))
                           #{}
                           exprs))]

    ;; The number of examples we're creating depends on how things were configured
    (log/info (describe-creation goal)
              "SCR examples [pos/neg]"
              (if all-pn?
                  "(emotive)"
                  "(includes stoic)"))

    ;; Shall we (pseudo)randomize the instances?
    (when-let [seed (cfg/?? :senti :rand-seed)]
      (log/fmt-info "Shuffling ~a input instances: seed[~a]" (.numInstances insts) seed)
      (.randomize insts (Random. seed)))

    ;; Throw away the counter & return the folded example sequence
    (second
      (reduce (fn [[cnts xmap :as info]
                   ^Instance inst]
                ;(log/debug "Counts:" cnts)
                ;; Do we have enough examples to stop?
                (if (creation-done? cnts goal)
                  (do (log/info "Examples:" cnts)
                      (reduced info))
                  (let [id     (long (.value inst COL-ID))
                        pole   (polarize inst)
                        pairs  (map #(str/split % #"_" 2)   ; Pairs are "pos_term"
                                     (str/split (.stringValue inst COL-TEXT) #" "))
                        terms  (map second pairs)
                        affect (map ->sense terms)          ; Affect: pos|neg|emo or nil per term
                        rules  (map ->scr   terms)]         ; Set of match-term rules per term

                    ;; Do we skip|process this Text??
                    (if (or (stoic? affect)                     ; Is it void of pos/neg/emotion?
                            (creation-full? cnts pole goal))    ; Still collecting for this polarity?
                      info
                      [(inc-pn-counter cnts dset pole)                  ; Update pos/neg/all counts
                       (update-values xmap                              ; Add Text for full set & all SCRs
                                      (apply set/union #{dset} rules)
                                      #(conj % {:id       id
                                                :polarity pole
                                                :content  terms
                                                :pos-tags (map first pairs)
                                                :rules    (map set/union rules affect)}))]))))

            [(zero-pn-counter dset)                                 ; Acc: total/pos/neg counts
             (reduce #(assoc %1 %2 #{}) {} (keys Expressions))]     ;      Examples keyed by rule

            (enumeration-seq (.enumerateInstances insts)))))))      ; Seq: Weka instances



;;; --------------------------------------------------------------------------
(defn create-scr-examples!
  "Create examples based on part-of-speech tokens.

  Example:  #3955 '- toothache subsiding, thank god for extra strength painkillers'

  Results in this entry being added to the @SCR-Examples value set under the key «DECREASE-N»:
            {:id 3955
             :polarity :positive
             :pos-tags («,» «N» «V»             «,»  «V»   «^» «P» «A» «N» «N»)
             :rules    (#{} #{} #{«DECREASE-N»} #{} #{«P»} #{} #{} #{} #{} #{})}"
  ([]
  (create-scr-examples! INIT-DATA-TAG))


  ([dset]
  (create-scr-examples! dset (ARFFs dset)))


  ([dset arff]
  (log/debug "Loading" dset "dataset" arff)
  (let [insts (weka/load-arff arff
                             (which-target))]
    ;; Create the new set of Text examples
    (reset! SCR-Examples
            (instances->examples dset insts))

    ;; Just tell them how many we have for each rule
    (update-values @SCR-Examples count))))



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
  (let [insts (rebase-data)
        rname (str (.relationName ^Instances Base-Instances)
                   (name tag))]
    (.setRelationName ^Instances insts rname)
    insts)))



;;; --------------------------------------------------------------------------
(defn rebase-data->hashmap
  "Creates a hashmap with the specified keys where every value is a Weka
  Instances object the say-senti base structure."
  [tags]
  (into {} (map #(vector % (rebase-data %))
                tags)))



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
(defn add-instance
  "Make a instance for a sentiment analysis ARFF dataset.

  NOTE: The format is defined buy (ARFFs :base) and hardcoded here!"
  ([^Instances  insts
    ^Instance   i]
  ;; Extract the values from the sample instance
  (add-instance insts
                (.value       i 0)
                (.stringValue i 1)
                (.value       i 2)))

  ([^Instances  insts
    ^Double     id
    ^String     text
    ^Double     sentiment]
  ;; Create an instance from values, append it to the dataset & return it
  (let [i (doto (DenseInstance. (.numAttributes (base-data)))
                (.setDataset insts)
                (.setValue 0 id)
                (.setValue 1 text)
                (.setValue 2 sentiment))]                       ; 0.0=neg, 1.0=pos
    (.add insts i)
    i)))



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
                    (add-instance insts
                                  (Float/parseFloat id)
                                  text
                                  (Float/parseFloat pn))))]     ; 0.0=neg, 1.0=pos

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
  ;; Pull what we need from the config before creating the cnt datasets
  (let [{:keys  [all-data? data-split lexicon text-index]
         :or    {data-split INIT-DATA-SPLIT
                 lexicon    INIT-LEX-TAG
                 text-index INIT-TEXT-INDEX}}     (cfg/? :senti)
        {:keys  [datasets parts train rand-seed]} data-split
        target  (inc (.classIndex (base-data)))                 ; 1-based dependent attribute index
        reattr  ["-R" (str (inc target) "-last," target)]]      ; Reorder filter opts: "4-last,3"

    ;; Sample (semi)full ARFF to create cnt train/test dataset pairs
    (into {}
      (domap
        (fn [n]
          (let [rseed    (+ rand-seed n)
                trn-tst  (split-data dtag rseed all-data? lexicon text-index reattr)
                arffs    (if (= dtag :weka)
                             ;; That's all the Weka Experimenter needs
                             trn-tst
                             ;; Chop trainers into parts for DL-Learner
                             (let [ftrain (:train trn-tst)
                                   subcnt (quot train parts)        ; Number of instances in a part
                                   extras (rem  train parts)        ; Leftovers from an uneven split
                                   iinsts (weka/load-arff ftrain)]  ; Reload the training instances

                               (log/info "Creating" parts "subsets of" subcnt "instances")
                               (when-not (zero? extras)
                                 (log/warn "Extra instances:" extras))

                               (assoc trn-tst
                                      :parts
                                      (domap
                                        #(weka/save-file (enumerate-dataset ftrain %)    ; Suffix: part num
                                                         (Instances. iinsts              ; Instances subset
                                                                     (int (* % subcnt))
                                                                     (int subcnt)))
                                        (range parts)))))]

            ;; We are building a hashmap
            [(key-prng rseed) arffs]))

        (range datasets)))))


  ([dtag seed all? lex tndx reattr]
  ;; This is the workhorse clause.  It is not meant to be called directly
  (let [rtag    (str "r" seed)
        goals   (split-pn-goals dtag)
        ipath   (if all?
                    (weka/tag-filename (ARFFs dtag) "COMPLETE" :arff)
                    (ARFFs dtag))
        iinsts  (weka/load-arff ipath (which-target))
        icnt    (.numInstances iinsts)

        dsets   (atom (rebase-data->hashmap SPLIT-TAGS))
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
                            (if (creation-full? cnts pn goal)
                              ;; Add this index to the "used" set, but don't add instance
                              (recur used* data-info)
                              (do ;(println "Adding to" (.relationName oinsts) "#" ndx)
                                  (add-instance oinsts inst)
                                  (recur used* [oinsts
                                                (inc-pn-counter cnts dtag pn)
                                                goal]))))))))

        wfilter (fn [data tt]
                  ;; Finalize the dataset for Weka
                  (let [emote   (tw/make-lexicon-filter lex tndx)
                        reorder (Reorder.)
                        insts*  (-> (data tt)
                                    (weka/filter-instances emote)               ; FIXME RE-running lexicon!
                                    (extend-data (weka/index1->0 tndx))         ; Insert POS counts
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
                      (.randomize insts rng)                        ; Distribute pos/neg more-or-less evenly
                      [tt (weka/save-file (ARFFs dtag)              ; Main filename stub
                                          (str rtag "." (name tt))  ; pRNG seed & train|test tag
                                          insts                     ; Sampled train|test data
                                          :arff)])
                    @dsets)))))



;;; --------------------------------------------------------------------------
(defn pn-examples
  "Returns a map of the IDs of tweets with positive polarities and those with
  negative polarities.  The caller may optionally specify a prefix for easy
  insertion into a DL-Learner configuration file."
  ([rule]
  (pn-examples rule ONT-PREFIX))

  ([rule prefix]
  (pn-examples rule prefix (get @SCR-Examples rule)))

  ([rule prefix examples]
  (let [tag     (str prefix (when prefix ":") TWEET-TAG)
        tagger  #(str tag %)
        ids     (reduce (fn [acc {:keys[id polarity]}]
                          (update-in acc [polarity]
                                         #(conj % id)))
                        {:positive (sorted-set)             ; Collect IDs for pos/neg examples
                         :negative (sorted-set)}
                        examples)
        xmps    (update-values ids #(map tagger %))]        ; Prefix % tag pos/neg IDs

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
  (if-let[ont (get @SCR-Ontologies tag)]

    ;; Right now, we're just testing with HermiT.
    (let [rsnr (make-reasoner :hermit ont)]
      (run! #(do (print (str %) ":")
                 (time (.precomputeInferences rsnr (into-array [%]))))
           Inferences))

    ;; Oops, someone skipped a step!
    (println "Please execute 'run' to create the ontologies"))))



;;; --------------------------------------------------------------------------
(defn read-solutions
  "Returns a vector of candidate solutions that were probably previously
  stored using process-solutions."
  ([]
  (read-solutions SOLN-LOG))


  ([fpath]
  (log/info "Reading DL-Learner solutions:" fpath)
    (with-open [rdr (io/reader fpath)]
       (seq (reduce (fn [solns txt]
                      (if-let [s (dll/read-solution txt)]
                        (conj solns s)
                        solns))
              []
              (line-seq rdr))))))



;;; --------------------------------------------------------------------------
(defn process-solutions
  "Handles candidate solutions.  This function is in FLUX...big time!!"
  [solns]
  (let [re-soln #(partial re-find %)
        [with
         without] (map re-soln [SOLN-WITH SOLN-WITHOUT])]

    ;; We're currently storing these in a file for processing by hand
    (with-open [ss (io/writer SOLN-LOG :append true)]
      (.write ss (str ";;; -" (java.util.Date.)
                      "-------------------------------------------\n"))
      (doseq [soln (filter with
                           (remove without solns))]
        (.write ss (str soln "\n"))))
    (log/info "Solutions saved to" SOLN-LOG)))



;;; --------------------------------------------------------------------------
(defn run
  "Runs a DL-Learner session to determine equivalent classes for Positive Texts."
  [& opts]
  ;; Shall we do some precleaning
  (when (some #{:reset} opts)
    (log/info "Recreating solutions file:" SOLN-LOG)
    (io/delete-file SOLN-LOG true))

  ;; Recreate our source ARFF if num-examples has been updated in the Config
  (when (some #{:arff} opts)
    (apply create-arffs opts))

  ;; Will this be Sentiment140 or another dataset?
  (let [base    "say-senti"
        dtag    (apply which-data opts)
        dpaths  (split-data dtag)

        check!  (fn [arff]
                  (log/debug "ARFF:" arff)
                  ;; (Re)generate the examples and ontologies from the ARFF
                  (create-scr-examples! dtag arff)
                  (populate-scr-ontologies!)
                  (save-ontologies)

                  ;; Do reasoner tests if requested
                  (when (some #{:timings} opts)
                    (run-timings))

                  ;; Run DL-Learner batch
                  (update-kv-values @SCR-Examples
                    (fn [rule xmps]
                      (dll/write-pn-config :base     base
                                           :rule     rule
                                           :prefixes (merge PREFIXES {"scr" (make-iri rule)})
                                           :examples (pn-examples rule "scr" xmps))))
                  (dll/run base dtag))]

    ;; For DL-Learner (non-weka), run the first of the data (sub)splits
    (if (some #{:weka} opts)
        (log/notice "Created" (count dpaths) "train/test ARFF pairs")   ; No run for Weka
        (->> (domap check! (:parts ((key-prng) dpaths)))                ; TODO: Run all splits
             (run! process-solutions)))))


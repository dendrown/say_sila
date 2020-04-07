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
            [clojure.pprint     :as prt :refer [pp pprint]]
            [tawny.english      :as dl]
            [tawny.reasoner     :as rsn]
            [tawny.repl         :as repl]                           ; <= DEBUG
            [tawny.owl          :refer :all])
  (:import  [java.util Random]
            [net.dendrown.uqam.hermit ConfigTools]                  ; TODO: Move HermiT to say.ontology
            [org.semanticweb.HermiT Configuration
                                    Configuration$TableauMonitorType
                                    Prefixes
                                    Reasoner]
            [org.semanticweb.HermiT.monitor TableauMonitorAdapter]
            [org.semanticweb.HermiT.tableau BranchingPoint
                                            Tableau]
            [org.semanticweb.owlapi.reasoner InferenceType]
            [weka.core DenseInstance
                       Instance
                       Instances]
            [weka.filters.unsupervised.attribute Reorder]))


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
;;; Default values for configuration elements
(def ^:const INIT-NUM-EXAMPLES  100)
(def ^:const INIT-DATA-TAG      :Sentiment140)
(def ^:const INIT-DATA-SPLIT    {:train 500, :test 500, :parts 10 :rand-seed 1})
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

(defonce EXPRESSIONS    (if (cfg/?? :senti :use-scr?)
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
                            {:no-rule        #{}}))



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


;; TODO: Move this into the SCR ontologies after Tawny gets functionality for OWLObjectMinCardinality.
;;       Also, we'll be automating the creation of this classed, based on output from DL-Learner.
(comment defclass PositiveTextCandidate
  :super    Text
  :label    "Positive Text Candidate"
  :comment  "A Text representing a candidate formula for determining if a given Text expresses popositive sentiment."
  :equivalent (dl/and Text
                      (at-least 2 (owl-oproperty)))) ;; TODO: "hasComponent ..."
  ;; DL-Learner: (dul/hasComponent min 2 (follows only (denotesAffect some Affect)))



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

;;; Add Test elements if we're evaluating the system
(when (cfg/?? :senti :testing?)
  (defclass PositiveTest
    :super   Affect
    :label   "Positive Test"
    :comment "This class is for system evaluation only.")
  (defpun PositiveTest))

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
(defonce Affect-Fragments   (into {} (map #(let [a (iri-fragment %)]
                                             [(lower-keyword a) a])
                                           (rsn/instances Affect))))
(defonce Affect-Names       (into #{} (vals Affect-Fragments)))



;;; --------------------------------------------------------------------------
;; TODO: Put SentimentPolarity back in after we handle timing considerations
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


;;; --------------------------------------------------------------------------
(defn which-data
  "Returns a data tag indicating which dataset has been requested in the
  specified options list."
  ([]
    INIT-DATA-TAG)

  ([opts]
  (let [datasets (into #{} (keys ARFFs))]
    (if-let [dtag (some datasets opts)]
      dtag
      (which-data)))))



;;; --------------------------------------------------------------------------
(defn which-datasets
  "Returns a sequence of tag indicating which datasets we use for evaluation."
  []
  (remove #{:base} (keys ARFFs)))



;;; --------------------------------------------------------------------------
(defn set-num-examples!
  "Defines the number of examples we should use from the source tweet data."
  [n]
  (cfg/!! :senti :num-examples n))



;;; --------------------------------------------------------------------------
(defn make-scr-iri
  "Creates a (String) IRI for the specified Sentiment Composition Rule (SCR)."
  [rule]
  (str ONT-ISTUB "-" (name rule) ".owl#"))



;;; --------------------------------------------------------------------------
(defn make-scr-ontology
  "Creates a version (copy) of the say-senti ontology, intended to include
  individuals expressing the specified Sentiment Composition Rule (SCR)"
  [rule]
  (let [scr    (name rule)
        prefix #(apply str % "-" scr %&)]
    (ontology
      :tawny.owl/name (prefix "say-senti")
      :iri     (make-scr-iri scr)
      :prefix  (prefix "scr")
      :import  say-senti
      :comment (str "Ontology for training sentiment models wrt. the Sentiment Composition Rule " scr))))



;;; --------------------------------------------------------------------------
(defprotocol Polarizer
  "Determines negative|positive polarity for various datatypes."
  (polarize [x] "Return the sentiment polarity as :positive or :negative."))

(extend-protocol Polarizer
  Number
  (polarize [x]
    (if (<= x 0.0)
        :negative
        :positive))

  Instance
  (polarize [inst]
    (polarize (.classValue inst)))


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
(defn- add-text
  "Adds a Text individual to the specified Sentiment Component Rule ontology."
  [ont
   {:keys [content id polarity pos-tags rules]}     ; Text (tweet) breakdown
   {:keys [full-links? pos-neg? testing?]}]         ; Configuration
  ;; The code will assume there's at least one token, so make sure!
  (when (seq pos-tags)
    (let [tid     (str TWEET-TAG id)
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

              ;; Add test affect to the first token if we're evaluating the system
              (when (and testing?
                         (= cnt 1)
                         (= polarity :positive))
                (express curr "PositiveTest"))

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
        (zip pos-tags rules)))))




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
(defn populate-scr-ontologies!
  "Populates the senti ontology using examples from the ARFFs"
  []
  (let [conf (cfg/? :senti)]                ; Freeze the configuration while we work
    ;; Create ontologies for each SCR, each populated with individuals expressing the rule
    (reset! SCR-Ontologies
            (update-kv-values @SCR-Examples
                              (fn [rule xmps]
                                (let [ont (make-scr-ontology rule)]
                                  (run! #(add-text ont % conf) xmps)
                                  ont))))
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
 (let [[goal
        checks] (if balance?
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
  (and balance? (>= (cnts pole) goal)))



;;; --------------------------------------------------------------------------
(defn- zero-pn-counter
  "Returns a map used to initialize counting SCR examples or data instances."
  [dset]
  (reduce #(assoc %1 %2 0) {} [dset :positive :negative]))



;;; --------------------------------------------------------------------------
(defn- inc-pn-counter
  "Returns an updated map after incrementing the couter values for the dataset
  and the specified polarity."
  [cnts dset pn]
  (update-values cnts [dset pn] inc))



;;; --------------------------------------------------------------------------
(defn create-scr-examples!
  "Create examples based on part-of-speech tokens.

  Example:  #3955 '- toothache subsiding, thank god for extra strength painkillers'

  Results in this entry being added to the @SCR-Examples value set under the key «DECREASE-N»:
            {:id 3955
             :polarity :positive
             :pos-tags («,» «N» «V»             «,»  «V»   «^» «P» «A» «N» «N»)
             :rules    (#{} #{} #{«DECREASE-N»} #{} #{«P»} #{} #{} #{} #{} #{})}"
  ([] (create-scr-examples! :Sentiment140))


  ([dset]
  (let [;;-- Keep track of how many examples to create, as per the configured 'balance' setting
        goal    (create-pn-goal dset)
        all-pn? (cfg/?? :senti :skip-neutrals?)
        stoic?  (fn [rules]                                     ; Check that we're not including neutral Texts
                  (and all-pn? (every? empty? rules)))          ; ..and that no sentiment (rule) is expressed

        ;;-- We bring in examples using Weka's Affective Tweets plugin and a Snowball stemmer
        arff    (ARFFs dset)
        insts   (weka/load-arff arff (cfg/?? :emote :target INIT-TARGET))
        sball   (tw/make-stemmer)
        stem    #(.stem sball %)
        exprs   (update-values EXPRESSIONS                      ; Pre-stem Liu's SCR expressions
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
              "SCR examples"
              (str "[pos/neg" (when-not all-pn? "/neu") "]"))

    ;; Shall we (pseudo)randomize the instances?
    (when-let [seed (cfg/?? :senti :rand-seed)]
      (log/fmt-info "Shuffling ~a input instances: seed[~a]" (.numInstances insts) seed)
      (.randomize insts (Random. seed)))

    ;; Create the new set of Text examples
    (reset! SCR-Examples
            (second
              (reduce (fn [[cnts xmap :as info]
                           ^Instance inst]
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
                     (reduce #(assoc %1 %2 #{}) {} (keys EXPRESSIONS))]     ;      Examples keyed by rule

                    (enumeration-seq (.enumerateInstances insts)))))        ; Seq: Weka instances

    ;; Just tell them how many we have for each rule
    (update-values @SCR-Examples count))))



;;; --------------------------------------------------------------------------
(defonce ^:private Base-Instances (weka/load-arff (:base ARFFs) INIT-TARGET))

(defn- ^Instances base-data
  "Returns the base say-senti data evaluation Instances"
  []
  Base-Instances)


(defn- ^Instances rebase-data
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
(defn add-instance
  "Make a instance for a sentiment analysis ARFF dataset.

  NOTE: The format is defined buy (ARFFs :base) and hardcoded here!"
  ([^Instances  insts
    ^Instance   i]
  ;; Extract the values from the sample instance
  (add-instance insts
                (.value    i 0)
                (.toString i 1)
                (.value    i 2)))

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
        conf    (cfg/? :senti)
        txt-ndx (get conf :text-index INIT-TEXT-INDEX)          ; 1-based index
        lex-tag (get conf :lexicon INIT-LEX-TAG)
        acnt    (.numAttributes (base-data))
        dsets   (atom {})                                       ; Collects datasets from CSV

        emoter  #(tw/make-lexicon-filter lex-tag txt-ndx)
        tagger  #(tw/make-tagging-filter txt-ndx)

        xform   (fn [iinsts & filters]
                  ;; Call to make a new Filter each time
                  (reduce
                    #(weka/filter-instances %1 (%2))
                    iinsts
                    (conj filters emoter)))

        line-up (fn [rdr]
                  (let [[_ & dlines] (csv/read-csv rdr)]        ; Skip CSV header
                    ;; The configuration may want to use a subset of the CSV data
                    (if-let [icnt (get conf :num-instances)]
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
                (let [oinsts (if (option? :weka opts)           ; When creating for Weka:
                                 (xform iinsts)                 ; - process lexicon
                                 (xform iinsts tagger))         ; - for OWL, also do POS tags
                      otag   (str dset suffix)]
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
  want to call directly)."
  ([]
    (split-data INIT-DATA-TAG))


  ([dtag]
    (split-data dtag 10))


  ([dtag cnt]
  ;; Pull what we need from the config before creating the cnt datasets
  (let [{:keys  [all-data? data-split lexicon text-index]
         :or    {data-split INIT-DATA-SPLIT
                 lexicon    INIT-LEX-TAG
                 text-index INIT-TEXT-INDEX}}   (cfg/? :senti)
        {:keys  [parts train rand-seed]}        data-split
        target  (inc (.classIndex (base-data)))                 ; 1-based dependent attribute index
        reattr  ["-R" (str (inc target) "-last," target)]]      ; Reorder filter opts: "4-last,3"

    ;; Sample (semi)full ARFF to create cnt train/test dataset pairs
    (doseq [c (range cnt)]
      (let [rseed         (+ rand-seed c)
            {arff :train} (split-data dtag rseed all-data? lexicon text-index reattr)]
        ;; That's all Weka needs, but for DL-Learner we chop the datasets into parts
        (when-not (= dtag :weka)
          (let [subcnt (int (quot train parts))                 ; Number of instances in a part
                extras (int (rem  train parts))                 ; Leftovers from an uneven split
                iinsts (weka/load-arff arff)]                   ; Reload the training instances

            ;; Split the [i]nput instances into several parts
            (dotimes [p parts]
              (weka/save-file arff
                              (strfmt "~3,'0d" p)                       ; Part number ARFF suffix
                              (Instances. iinsts (* p subcnt) subcnt)   ; Subset of instances
                              :arff))

            (log/info "Created" parts "subsets of" subcnt "instances")
            (when-not (zero? extras)
              (log/warn "Extra instances:" extras))))))))


  ([dtag seed all? lex tndx reattr]
  ;; This is the workhorse clause.  It is not meant to be called directly
  (let [rtag    (str "r" seed)
        goals   (split-pn-goals dtag)
        ipath   (if all?
                    (weka/tag-filename (ARFFs dtag) "COMPLETE" :arff)
                    (ARFFs dtag))
        iinsts  (weka/load-arff ipath (cfg/?? :emote :target INIT-TARGET))
        icnt    (.numInstances iinsts)

        dsets   (atom (rebase-data->hashmap SPLIT-TAGS))
        cntr    (conj dtag)
        rng     (Random. seed)
        fill    (fn [used [^Instances oinsts cnts goal
                           :as data-info]]
                  (if (creation-done? cnts goal)
                    ;; We've got what we need, the used-index set is the accumulator
                    used
                    ;; Keep pulling from the input data
                    (let [ndx (.nextInt rng icnt)]
                      (if (contains? used ndx)
                          (do ;(log/debug "Resampling on repeat index:" ndx)
                              (recur used data-info))
                          (let [inst  (.get iinsts ndx)
                                pn    (polarize inst)
                                used! (conj used ndx)]
                            (if (creation-full? cnts pn goal)
                              ;; Add this index to the "used" set, but don't add instance
                              (recur used! data-info)
                              (do ;(println "Adding to" (.relationName oinsts) "#" ndx)
                                  (add-instance oinsts inst)
                                  (recur used! [oinsts
                                                (inc-pn-counter cnts dtag pn)
                                                goal]))))))))

        wfilter (fn [data tt]
                  (let [emote   (tw/make-lexicon-filter lex tndx)
                        reorder (Reorder.)
                        insts*  (-> (data tt)
                                    (weka/filter-instances emote)
                                    (weka/filter-instances reorder reattr))]
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
    ;; Note, this updates the dsets agent (concurrently) with new Instances.
    (when (= dtag :weka)
      (log/debug "Filtering datasets for Weka")
      (run! #(swap! dsets wfilter %) SPLIT-TAGS))

    ;; Save the output train/test datasets & return a map of the ARFF paths
    (into {} (domap (fn [[tt insts]]
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
(defn make-monitor
  "Creates a custom monitor for a HemiT reasoner."
  [& opts]
  ;; FIXME: Move this to say.ontology when stable
  (let [tableau   (atom nil)
        start-ms  (atom 0)
        forms     (agent {})
        log-limit (agent 2000)
        log       #(when (pos? @log-limit)
                     (apply println %&)
                     (send log-limit dec))
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
                (println "ABox satisfiability checked in" (- (System/currentTimeMillis) @start-ms) "ms")
                (println "Checks on individuals:")
                (pprint fcnts)
                (println "TOTAL:" (reduce + 0 (vals fcnts))))))

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
                        #(let [cnt (get % lvar 0)]                 ; How many times have we seen this variable?
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

  ([rkey ont]
  (show-reasoning rkey ont)))



;;; --------------------------------------------------------------------------
(defn run-timings
  "Performs timing checks for an ontology (defaults to :Sentiment140)."
  ([]
  (run-timings :Sentiment140))

  ([tag]
  (if-let[ont (get @SCR-Ontologies tag)]

    ;; Right now, we're just testing with HermiT.
    (let [rsnr (make-reasoner :hermit ont)]
      (map #(do (print (str %) ":")
                (time (.precomputeInferences rsnr (into-array [%]))))
           Inferences))

    ;; Oops, someone skipped a step!
    (println "Please execute 'run' to create the ontologies"))))



;;; --------------------------------------------------------------------------
(defn run
  "Runs a DL-Learner session to determine equivalent classes for Positive Texts."
  [& opts]
  ;; Recreate the ARFF if num-examples has been updated in the Config
  (when (some #{:arff} opts)
    (apply create-arffs opts))

  ;; Use ARFF to generate the examples and ontologies
  (create-scr-examples!)
  (populate-scr-ontologies!)
  (save-ontologies)
  (update-kv-values @SCR-Examples
    (fn [rule xmps]
      (dll/write-pn-config :base     "say-senti"
                           :rule     rule
                           :prefixes (merge PREFIXES {"scr" (make-scr-iri rule)})
                           :examples (pn-examples rule "scr" xmps))))

  ;; Do tests if requested
  (when (some #{:timings} opts)
    (run-timings)))


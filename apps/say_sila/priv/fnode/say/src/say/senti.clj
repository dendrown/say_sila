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
            [clojure.core.match :refer [match]]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :as prt :refer [pp pprint]]
            [tawny.english      :as dl]
            [tawny.reasoner     :as rsn]
            [tawny.repl         :as repl]                           ; <= DEBUG
            [tawny.owl          :refer :all])
  (:import  [net.dendrown.uqam.hermit ConfigTools]                  ; TODO: Move HermiT to say.ontology
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
            [weka.filters.unsupervised.attribute TweetNLPPOSTagger
                                                 TweetToSentiStrengthFeatureVector]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-ISTUB  "http://www.dendrown.net/uqam/say-senti")
(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-senti.owl#")
(def ^:const ONT-FSTUB  "resources/KB/say-senti")
(def ^:const ONT-FPATH  "resources/KB/say-senti.owl")
(def ^:const ONT-PREFIX "senti")
(def ^:const DATASET    "resources/emo-sa/sentiment-analysis.csv")
(def ^:const ARFFs      {:base          "resources/emo-sa/sentiment-analysis.arff"
                         :Sentiment140  "resources/emo-sa/sentiment-analysis.Sentiment140.arff"
                         :Kaggle        "resources/emo-sa/sentiment-analysis.Kaggle.arff"})
(def ^:const COL-ID     0)
(def ^:const COL-TEXT   1)

(def ^:const TWEET-TAG  "t")                        ; Tweet individual have this tag plus the ID ( "t42" )
(def ^:const PREFIXES   {"senti"    ONT-IRI
                         "pos"      pos/ONT-IRI})

(def ^:const INIT-NUM-EXAMPLES  100)


;;; --------------------------------------------------------------------------
;; TODO: Debugging, remove this soon!
(defonce Inferences (into-array [InferenceType/CLASS_HIERARCHY
                                 InferenceType/CLASS_ASSERTIONS
                                 InferenceType/OBJECT_PROPERTY_HIERARCHY
                                 InferenceType/DATA_PROPERTY_HIERARCHY
                                 InferenceType/OBJECT_PROPERTY_ASSERTIONS
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

;; Bring in the DOLCE+DnS Ultralite foundational ontology as configured
(dul/access)


;;; --------------------------------------------------------------------------
(owl-import pos/cmu-pos)
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

(defcopy pos/Token)
(refine Token :equivalent Term) ;(dl/or Term Punctuation))

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

;; TODO: Verify in \liu2015 if P/N refer to a pos/neg modifier or a pos/neg aspect
(defscr P "An atomic, nonterminal sentiment composition expressing positive sentiment.")
(defscr N "An atomic, nonterminal sentiment composition expressing negative sentiment.")
(when (cfg/?? :senti :use-scr?)
  (log/notice "Creating Sentiment Composition Rules")
  (defscr DECREASE-N  "Expressions which decrease NPI and NE terms.")
  (defscr DECREASE-P  "Expressions which decrease PPI and PO terms.")
  (defscr INCREASE-N  "Expressions which increase NPI and NE terms.")
  (defscr INCREASE-P  "Expressions which increase PPI and PO terms."))


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
  Object
  (polarize [x]
    (if (<= x 0.0)
        :negative
        :positive))

  Instance
  (polarize [inst]
    (polarize (.classValue inst))))


;;; --------------------------------------------------------------------------
(defn- add-text
  "Adds a Text individual to the specified Sentiment Component Rule ontology."
  [ont
   {:keys [id polarity pos-tags rules]}     ; Text (tweet) breakdown
   {:keys [full-links? pos-neg?]}]          ; Configuration
  ;; The code will assume there's at least one token, so make sure!
  (when (seq pos-tags)
    (let [tid     (str TWEET-TAG id)
          text    (individual ont tid
                    :type (if pos-neg?
                              (case polarity :negative NegativeText
                                             :positive PositiveText)
                              Text))
          express #(refine ont %1 :fact (is dul/expresses (individual ont %2)))]

      ;; Note that we're receiving the Texts in reverse order
      (when (zero? (rem id 100))
        (log/debug "Processing" id "texts..."))

      ;; Add an entity representing the text itself.  Note that we'll be creating
      ;; the referenced token "tN-1" in the reduce expression below.

      ;; And entities for each of the terms, linking them together and to the text
      (reduce
        (fn [[cnt tokens :as info]
             [tag rules]]
          ;; Get the Part of Speech for the tag reported by Weka
          (if-let [pos (pos/lookup# tag)]

            ;; Set up an individual for this Token
            (let [ttid (str tid "-" cnt)
                  curr (individual ont ttid
                                   :type  Token
                                   :label (str ttid " (" tag ")"))]

              ;; Link Token to the original Text and set POS Quality
              (refine ont text :fact (is dul/hasComponent curr))
              (refine ont curr :fact (is pos/isPartOfSpeech pos))

            ;; Link tokens to each other
            (when-let [prev (first tokens)]
              (refine ont curr :fact (is dul/directlyFollows prev))

              ;; The reasoner can figure out the rest, but being explicit may be faster
              (when full-links?
                ;; The current Token comes after all the tokens we've seen so far
                (refine ont prev :fact (is dul/directlyPrecedes curr))
                (domap (fn [tok]
                         (refine ont curr :fact (is dul/follows tok))
                         (refine ont tok  :fact (is dul/precedes curr)))
                       tokens)))

            ;; Express sentiment composition rules
            (doseq [rule rules]
              (when-not (contains? #{"P" "N"} rule)
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
                                  (domap #(add-text ont % conf) xmps)
                                  ont))))
    (keys @SCR-Ontologies)))



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
        bal?    (cfg/?? :senti :balance?)
        xcnt    (cfg/?? :senti :num_examples INIT-NUM-EXAMPLES) ; Base e[x]ample count
        [goal
         chks]  (if bal?
                    [(int (/ xcnt 2)) [:positive :negative]]    ; Count pos/neg instances separately
                    [xcnt [dset]])                              ; Count all instances together
        done?   (fn [cnts]                                      ; Termination checker using example counts
                  (every? #(>= (cnts %) goal) chks))
        full?   (fn [cnts pole]                                 ; Check if we have enough pos|neg Texts
                  (and bal? (>= (cnts pole) goal)))

        ;;-- We bring in examples using Weka's Affective Tweets plugin and a Snowball stemmer
        arff    (ARFFs dset)
        insts   (weka/load-arff arff "sentiment")
        sball   (tw/make-stemmer)
        stem    #(.stem sball %)
        exprs   (update-values EXPRESSIONS                      ; Pre-stem Liu's SCR expressions
                               #(into #{} (map stem %)))

        ;;-- Functions to identify pos/neg tokens and Sentiment composition rules
        lex     (tw/make-pn-lexicon :bing-liu)
        ->pn  #(case (.retrieveValue lex %)                     ; Lexicon lookup for P/N rules
                 "positive"  "P"
                 "negative"  "N"
                 "not_found" nil)
        ->scr #(let [term (stem %)]                             ; Match terms for Sentiment Composite Rules
                 (reduce (fn [acc [scr terms]]
                           (if (contains? terms term)
                               (conj acc scr)
                               acc))
                         #{}
                         exprs))
        unite #(if %2 (conj %1 %2) %1)]                         ; Accepts a P|N (%2) into a set of SCRs (%1)

    ;; The number of examples we're creating depends on how things were configured
    (log/info (if bal? (str "Balancing " goal "/" goal)
                       (str "Creating "  goal))
              (name dset)
              "SCR examples")

    ;; Create the new set of Text examples
    (reset! SCR-Examples
            (second
              (reduce (fn [[cnts xmap :as info]
                           ^Instance inst]
                        ;; Do we have enough examples to stop?
                        (log/debug "CNTS:" cnts)
                        (if (done? cnts)
                          (reduced info)
                          (let [id    (long (.value inst COL-ID))
                                pole  (polarize inst)
                                pairs (map #(str/split % #"_" 2)    ; Pairs are "pos_term"
                                            (str/split (.stringValue inst COL-TEXT) #" "))
                                terms (map second pairs)
                                pns   (map ->pn  terms)             ; Single P|N rule or nil per term
                                rules (map ->scr terms)]            ; Set of match-term rules per term

                            ;; Are we still collecting for this polarity
                            (if (full? cnts pole)
                              info
                              [(update-values cnts [pole dset] inc)             ; Update pos/neg/all counts
                               (update-values xmap                              ; Add Text for full set & all SCRs
                                              (apply set/union #{dset} rules)
                                              #(conj % {:id       id
                                                        :polarity pole
                                                        :pos-tags (map first pairs)
                                                        :rules    (map unite rules pns)}))]))))

                    [(reduce #(assoc %1 %2 0)   {} [dset :positive :negative])  ; Acc: total/pos/neg counts
                     (reduce #(assoc %1 %2 #{}) {} (keys EXPRESSIONS))]         ;      Examples keyed by rule

                    (enumeration-seq (.enumerateInstances insts)))))    ; Seq: Weka instances

    ;; Just tell them how many we have for each rule
    (update-values @SCR-Examples count))))



;;; --------------------------------------------------------------------------
(defn create-arffs
  "Initial function to create the senti ontology.  Expect changes."
  ([] (create-arffs DATASET))

  ([fpath]
  (let [dsets   (atom {})
        base    (weka/load-arff (:base ARFFs))
        rname   (.relationName base)
        acnt    (.numAttributes base)
        icnt    (cfg/?? :senti :num-instances INIT-NUM-EXAMPLES)

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
         ;; TODO use a subset until we get everything squared away
         (doseq [line (take icnt dlines)]
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
    (spit (str "resources/emo-sa/pn-examples-" rule ".edn")
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
    (create-arffs))

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


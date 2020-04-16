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
;;;; @copyright 2019-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.filters.unsupervised.attribute.TweetToGenderFeatures
  (:require [say.genie       :refer :all]
            [say.log         :as log]
            [say.social      :as soc]
            [weka.core       :as weka]
            [clojure.set     :as set]
            [clojure.string  :as str])
  (:import  (affective.core ArffLexiconEvaluator)
            (weka.core Attribute
                       DenseInstance
                       Instance
                       Instances
                       Option
                       SingleIndex
                       Utils
                       WekaEnumeration)
            [weka.filters.unsupervised.attribute TweetToGenderFeatures  ; (this)
                                                 TweetToFeatureVector])
  (:gen-class
    :name       weka.filters.unsupervised.attribute.TweetToGenderFeatures
    :extends    weka.filters.unsupervised.attribute.TweetToFeatureVector
    :state      state
    :init       init
    :methods    [[getScreenNameIndex    []        String]
                 [getFullNameIndex      []        String]
                 [getDescriptionIndex   []        String]
                 [isUsesEMNLP2014       []        Boolean]
                 [setScreenNameIndex    [String]  void]
                 [setFullNameIndex      [String]  void]
                 [setDescriptionIndex   [String]  void]
                 [setUsesEMNLP2014      [boolean] void]
                 [setUpperIndices       [int]     void]]
    :exposes    {m_textIndex {:get superGetTextIndex
                              :set superSetTextIndex}}      ; FIXME: name dup
    :exposes-methods {;- weka.filters.Filter ---------------------------------
                      getOptions    superGetOptions
                      setOptions    superSetOptions
                      listOptions   superListOptions
                      ;- TweetToFeatureVector --------------------------------
                      setTextIndex  superSetTextIndex}))    ; FIXME: name dup

(set! *warn-on-reflection* true)

;;; Erlang handles downloading tweets and preparing ARFFs (pan.erl and twitter.erl)
(def ^:const GENDER-GOLD "/srv/say_sila/weka/gender.pan2014.1-2.arff")      ; Subset!!

(def ^:const GENDERS                [:female :male])
(def ^:const SUPER-OPT-TEXT-NDX     "I")
(def ^:const OPT-SCREEN-NAME-NDX    "S")
(def ^:const OPT-FULL-NAME-NDX      "N")
(def ^:const OPT-DESCRIPTION-NDX    "D")
(def ^:const OPT-USES-EMNLP-2014    "E")

(def ^:const INDEX-OPT-KEYS         [SUPER-OPT-TEXT-NDX     ; Define an input index (column)
                                     OPT-SCREEN-NAME-NDX
                                     OPT-FULL-NAME-NDX
                                     OPT-DESCRIPTION-NDX])

(def ^:const NAMES-OPT-NDX-KEYS     [OPT-SCREEN-NAME-NDX    ; Input indexes for F/M name checks
                                     OPT-FULL-NAME-NDX
                                     OPT-DESCRIPTION-NDX])

(def ^:const LEXICON-OPT-KEYS       [OPT-USES-EMNLP-2014])  ; Flags for lexicon use


(def ^:const LEXICON-OPT-NDX-KEYS   [SUPER-OPT-TEXT-NDX     ; Input indexes to check against lexicons
                                     OPT-DESCRIPTION-NDX])


(def Options {OPT-SCREEN-NAME-NDX   (Option. "The column index that holds users' screen names."
                                             OPT-SCREEN-NAME-NDX 1 "-S <col>")

              OPT-FULL-NAME-NDX     (Option. "The column index that holds users' full names"
                                             OPT-FULL-NAME-NDX 1 "-N <col>")

              OPT-DESCRIPTION-NDX   (Option. "The column index that holds users' profile description text."
                                             OPT-DESCRIPTION-NDX 1 "-D <col>")

              OPT-USES-EMNLP-2014   (Option. (str "Uses the EMNLP-2014 gender lexicon to calculate a gender"
                                                  "score based on the column specified by the Text Index option.")
                                             OPT-USES-EMNLP-2014 0 "-E")})


;;; Lexicon data is prepared in say.data
(defonce NAMES       (read-string (slurp "resources/gender/names.edn")))    ; http://www.20000-names.com
(defonce EMNLP-2014  (read-string (slurp "resources/gender/emnlp14.edn")))  ; https://github.com/wwbp/lexica
(def ^:const EMNLP-BIAS  -0.06724152)                                       ; Gender model: male < 0 < female


;; ---------------------------------------------------------------------------
(defn -init
  "
  Initializes this filter's state.  Our parent ( TweetToFeatureVector )
  initializes the Tweet text index to column 1.  Even though the columns
  we care about will likely come first, we don't want to override this
  default.  Therefore, we assume nothing.
  "
  []
  [[] (atom {})])



;; ---------------------------------------------------------------------------
(defn get-state
  "
  Updates the filter's state with the specified key-value pair.
  "
  [^TweetToGenderFeatures this field]
  (@(.state this) field))



;; ---------------------------------------------------------------------------
(defn set-state!
  "
  Updates the filter's state with the specified key-value pair.  The value (x)
  may simply be data or else a function that determines the value to associate
  with the field in this object's state.
  "
  [^TweetToGenderFeatures this field x]
  (swap! (.state this) update field
                              (if (fn? x) x (fn[_] x))))



;; ---------------------------------------------------------------------------
(defn- get-index
  "
  Returns the setting for the specified (one-based) column index.
  "
  [this opt]
  (let [ndx ^SingleIndex (get-state this opt)]
    (.getSingleIndex ndx)))



;; ---------------------------------------------------------------------------
(defn -getScreenNameIndex
  "
  Returns the setting for the (one-based) column index that holds users'
  screen names.
  "
  [this]
  (get-index this OPT-SCREEN-NAME-NDX))



;; ---------------------------------------------------------------------------
(defn -getFullNameIndex
  "
  Returns the setting for the (one-based) column index dedicated to the users'
  full names.
  "
  [this]
  (get-index this OPT-FULL-NAME-NDX))



;; ---------------------------------------------------------------------------
(defn -getDescriptionIndex
  "
  Returns the setting for the (one-based) column index that holds users'
  profile description
  "
  [this]
  (get-index this OPT-DESCRIPTION-NDX))



;; ---------------------------------------------------------------------------
(defn -isUsesEMNLP2014
  "
  Returns whether or not this Filter uses the EMNLP-2014 gender lexicon.
  "
  [this]
  (get-state this OPT-USES-EMNLP-2014))



;; ---------------------------------------------------------------------------
(defn- set-index
  "
  Sets the specified (one-based) column index.
  "
  [this opt col]
  (when col
    (set-state! this opt (fn [_] (SingleIndex. (str col))))))



;; ---------------------------------------------------------------------------
(defn -setScreenNameIndex
  "
  Sets the (one-based) column index that holds users' screen names.
  "
  [this col]
  (set-index this OPT-SCREEN-NAME-NDX col))



;; ---------------------------------------------------------------------------
(defn -setFullNameIndex
  "
  Sets the (one-based) column index which holds users' full names.
  "
  [this col]
  (set-index this OPT-FULL-NAME-NDX col))



;; ---------------------------------------------------------------------------
(defn -setDescriptionIndex
  "
  Sets the (one-based) column index that holds users' profile description.
  "
  [this col]
  (set-index this OPT-DESCRIPTION-NDX col))



;; ---------------------------------------------------------------------------
(defn -setUsesEMNLP2014
  "
  Instructs this Filter to use the EMNLP-2014 gender lexicon.
  "
  [this use?]
   ;;
   ;; TODO: Using the lexicon should invoke lower case mode for the tokenizer
   ;;       Right?
   ;;
  (set-state! this
              OPT-USES-EMNLP-2014
              (fn [_] use?)))



;; ---------------------------------------------------------------------------
(defn -setUpperIndices
  "
  Sets the upper index for anything column-related in the Filter state.
  "
  [^TweetToGenderFeatures this col]
  (let [tindex (.superGetTextIndex this)]
    (.setUpper ^SingleIndex tindex col)
    (swap! (.state this) (fn [state]
                           ;; We're just mutating the state's Weka objects
                           (doseq [[_ ndx] (select-keys state INDEX-OPT-KEYS)]
                             (.setUpper ^SingleIndex ndx col))
                           state))))



;; ---------------------------------------------------------------------------
(defn -getOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this]
  (let [->cli #(str "-"  %)
        state @(.state this)]
    ;; Push our options onto whatever our parent reports
    (as-> (reduce (fn [opts [tag ^SingleIndex ndx]]         ; Add -X <col> options
                     (conj opts (.getSingleIndex ndx)       ; in reverse order
                                (->cli tag)))
                 (seq (.superGetOptions this))
                 (select-keys state NAMES-OPT-NDX-KEYS)) opt-seq

         (reduce (fn [opts [tag use?]]                      ; Add lexicon options
                   (if use?
                       (conj opts (->cli tag))
                       opts))
                 opt-seq
                 (select-keys state LEXICON-OPT-KEYS))

         (into-array String opt-seq))))                     ; Java-ize the output



;; ---------------------------------------------------------------------------
(defn -setOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this
   ^"[Ljava.lang.String;" opts]

  ;; CAREFUL: The calls to Weka Utils will mutate the opts array
  (let [get-opt #(not-empty (Utils/getOption ^String % opts))
        is-opt? #(Utils/getFlag ^String % opts)]

    (when-let [ndx (get-opt OPT-SCREEN-NAME-NDX)]   (.setScreenNameIndex  this ndx))
    (when-let [ndx (get-opt OPT-FULL-NAME-NDX)]     (.setFullNameIndex    this ndx))
    (when-let [ndx (get-opt OPT-DESCRIPTION-NDX)]   (.setDescriptionIndex this ndx))

    (when (is-opt? OPT-USES-EMNLP-2014)
      (.setUsesEMNLP2014 this true)))

  ;; Whatever's left is for the parent Filter
  (.superSetOptions this opts))



;; ---------------------------------------------------------------------------
(defn -listOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this]
  (WekaEnumeration. (concat (enumeration-seq (.superListOptions this))
                            (vals Options))))


;; ---------------------------------------------------------------------------
(defn -globalInfo
  "
  Returns a string description of this filter
  "
  [this]
  "A Filter to count female/male names in Instance Attributes.")


;; ---------------------------------------------------------------------------
(defn tag-column
  "
  Returns a string describing a filtered, gender-distinguishing column
  "
  [prefix opt gnd]
  (str (name prefix) "-" opt "-" (name gnd)))



;; ---------------------------------------------------------------------------
(defn tag-name
  "
  Returns a string describing a name-distinguishing column or other entity.
  "
  [opt gnd]
  (tag-column :name opt gnd))



;; ---------------------------------------------------------------------------
(defn tag-gender
  "
  Returns a string describing a name-distinguishing column or other entity.
  "
  [opt ndx]
  (tag-column :gender opt ndx))



;; ---------------------------------------------------------------------------
(defn -determineOutputFormat
  "
  Returns a empty Instances object, which defines the filter's output after
  processing a set of input Instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             insts]

  ;; Set up a reference for the TextIndex we inherent from our parent class.
  ;; We use that data member, and it facilitates the text processing to keep
  ;; it with the rest of our indices. (Ideally, we'd do this in -init, but
  ;; we don't have access to the parent object there
  (set-state! this SUPER-OPT-TEXT-NDX (.superGetTextIndex this))

  (let [acnt     (.numAttributes insts)
        result   (Instances. insts 0)
        append   #(.insertAttributeAt result (Attribute. (apply %1 %&))
                                             (.numAttributes result))
        state    @(.state this)]

    ;; Make sure the user stays in bounds for the attributes we hit
    (.setUpperIndices this (dec acnt))

    ;; TODO: We're doing double fields for name cchecks. Compare this format
    ;;       to a single field using female(pos-vals) and male(neg-vals).
    (doseq [opt (get-keys state NAMES-OPT-NDX-KEYS)
            gnd GENDERS]
        (append tag-name opt gnd))

    ;; Are we using a lexicon?
    (when (.isUsesEMNLP2014 this)
      ;; Always process the (super) text index...plus our own selected indices
      (doseq [opt (get-keys state LEXICON-OPT-NDX-KEYS)]
        (append tag-gender OPT-USES-EMNLP-2014 opt)))

    result))



;; ---------------------------------------------------------------------------
(defn -process
  "
  Run the filter on the input instances and return the output instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             insts]

  ;; NOTE: the result Instances are iteratively mutated in the Java way...
  ;; Also, several variables are prefixed with:
  ;;    i - for input attributes
  ;;    o - for output (result) attributes
  (let [state    @(.state this)
        result   (Instances. ^Instances (.determineOutputFormat this insts) 0)
        icnt     (.numAttributes insts)
        ocnt     (.numAttributes result)
        in-index #(when-let [^SingleIndex ndx (state %)]
                        (.getIndex ndx))
        fm-index #(weka/get-index result (tag-gender %1 %2))

        tokenize (memoize (fn [txt]
                            (jcall affective.core.Utils/tokenize
                                   txt
                                   [this .isToLowerCase
                                         .isStandarizeUrlsUsers
                                         .isReduceRepeatedLetters
                                         .getTokenizer
                                         .getStemmer
                                         .getStopwordsHandler])))
        [_ opts] (reduce (fn [[out indices] opt]
                           (if-let [in (in-index opt)]
                             ;; The accumulator is a pair:
                             ;; [0] the next output column
                             ;; [1] a column-finder map {K=opt-code, V=[in-col out-col]
                             [(+ out 2) (assoc indices opt [in out])]       ; Use it
                             [out indices]))                                ; Skip it
                         [icnt {}]
                         NAMES-OPT-NDX-KEYS)]

    (log/debug "Local index options:" opts)
    (log/debug "Post-filter attribute count:" ocnt)
    (letfn [;-----------------------------------------------------------------
            (attr-str [^Instance inst a]
              (.stringValue inst (int a)))

            ;-----------------------------------------------------------------
            (count-names [words gnd]
              (count (set/intersection (set words) (NAMES gnd))))

            ;-----------------------------------------------------------------
            (set-counts [^doubles ovals oi toks]
              (aset ovals oi       (double (count-names toks :female)))
              (aset ovals (inc oi) (double (count-names toks :male))))

            ;-----------------------------------------------------------------
            (->tokens [inst a]
              (let [tokens (tokenize (attr-str inst a))]
                (log/info "TOKENS:" (str tokens))
                tokens))

            ;-----------------------------------------------------------------
            (inc-hit [hits word weight]
              (let [[hcnt _] (get hits word [0 :none])]
                (assoc hits word [(inc hcnt) weight])))

            ;-----------------------------------------------------------------
            (check-emnlp [^Instance inst a]
              (reduce (fn [[wcnt hits] word]
                        [(inc wcnt)                             ; [0] Always another word
                         (if-let [weight (EMNLP-2014 word)]     ; [1] Check lexicon hits
                           (inc-hit hits word weight)
                           hits)])
                      [0 {}]
                      (->tokens inst a)))]

      ;; Run through all the instances...
      (doseq [^Instance inst (seq insts)]
        (let [ovals (double-array ocnt)]

          ;; INPUT ATTRS: just copy in the attribute values across the input columns
          (dotimes [a icnt]
            (aset ovals a (.value inst a)))

          ;; OUTPUT ATTRS:
          ;;
          ;; Got F/M names jumbled in the screen name?
          (when-let [[in out] (opts OPT-SCREEN-NAME-NDX)]
            (set-counts ovals out (soc/tokenize (attr-str inst in) :lower-case)))

          ;; Got F/M names in the name or the profile description?
          (when-let [[in out] (opts OPT-FULL-NAME-NDX)]   (set-counts ovals out (->tokens inst in)))
          (when-let [[in out] (opts OPT-DESCRIPTION-NDX)] (set-counts ovals out (->tokens inst in)))

          ;; Check a lexicon?
          (when (.isUsesEMNLP2014 this)
            (doseq [opt (get-keys state LEXICON-OPT-NDX-KEYS)]
              (let [[wcnt
                     hits] (check-emnlp inst (in-index opt))
                    out    (fm-index OPT-USES-EMNLP-2014 opt)
                    score  (double (reduce (fn [acc [word [hcnt weight]]]
                                             (log/fmt-debug "Score<~a>: ~6$ + (~a + ~6$)/~a <= ~a"
                                                            opt acc hcnt weight wcnt word)
                                             (+ acc (/ (* hcnt weight)
                                                        wcnt)))
                                           0.00
                                           hits))]
                (aset ovals out score))))

          ;; Append the finished instance to the output dataset
          (.add result (DenseInstance. 1.0 ovals)))))

    ;; ...and we're done!
    result))

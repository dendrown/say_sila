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
            [say.social      :as soc]
            [say.weka        :as weka]
            [clojure.set     :as set]
            [clojure.string  :as str])
  (:import  [affective.core ArffLexiconEvaluator]
            [weka.core Attribute
                       Capabilities
                       Capabilities$Capability
                       DenseInstance
                       Instance
                       Instances
                       Option
                       SingleIndex
                       Utils
                       WekaEnumeration]
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
                              :set superGetTextIndex}}
    :exposes-methods {;- weka.filters.Filter ---------------------------------
                      getOptions    superGetOptions
                      setOptions    superSetOptions
                      listOptions   superListOptions
                      ;- TweetToFeatureVector --------------------------------
                      setTextIndex  superSetTextIndex}))

(set! *warn-on-reflection* true)

;;; Erlang handles downloading tweets and preparing ARFFs (pan.erl and twitter.erl)
(def ^:const GENDER-GOLD "/srv/say_sila/weka/gender.pan2014.1-2.arff")      ; Subset!!

(def ^:const OPT-SCREEN-NAME-NDX    "S")
(def ^:const OPT-FULL-NAME-NDX      "N")
(def ^:const OPT-DESCRIPTION-NDX    "D")
(def ^:const OPT-USES-EMNLP-2014    "E")
(def ^:const INDEX-OPT-KEYS         [OPT-SCREEN-NAME-NDX                    ; Defines option order
                                     OPT-FULL-NAME-NDX                      ; (but not all may be
                                     OPT-DESCRIPTION-NDX])                  ; selected)
(def ^:const LEXICON-OPT-KEYS       [OPT-USES-EMNLP-2014])
(def ^:const OPT-KEYS               (concat INDEX-OPT-KEYS LEXICON-OPT-KEYS))

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
  Updates the filter's state with the specified key-value pair.
  "
  [^TweetToGenderFeatures this field fun]
  (swap! (.state this) update field fun))



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
  (set-state! this opt (fn [_] (SingleIndex. (str col)))))



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
                 (select-keys state INDEX-OPT-KEYS)) opt-seq

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
(defn tag-name
  "
  Returns a string describing a name-distinguishing column or other entity.
  "
  [opt gnd]
  (str "name-" opt "-" (name gnd)))



;; ---------------------------------------------------------------------------
(defn tag-gender
  "
  Returns a string describing a name-distinguishing column or other entity.
  "
  [opt]
  (str "gender-" opt))



;; ---------------------------------------------------------------------------
(defn -determineOutputFormat
  "
  Returns a empty Instances object, which defines the filter's output after
  processing a set of input Instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             insts]
  (let [state    @(.state this)
        acnt     (.numAttributes insts)
        result   (Instances. insts 0)]

    ;; Make sure the user stays in bounds for the attributes we hit
    (.setUpperIndices this (dec acnt))

    ;; TODO: We're doing double fields for each category.  Compare this format
    ;;       to a single field using female(pos-vals) and male(neg-vals).
    (loop [col  acnt
           opts INDEX-OPT-KEYS]
      (when-let [opt (first opts)]
        (recur (if (state opt)
                 ;; We're using this option, add the appropriate attributes 
                 (do (.insertAttributeAt result (Attribute. (tag-name opt :female)) col)
                     (.insertAttributeAt result (Attribute. (tag-name opt :male)) (inc col))
                     (+ col 2))
                 ;; This option is unset, no extra attributes
                 col)
               (rest opts))))

    ;; Are we using any lexicons?
    (when (.isUsesEMNLP2014 this)
      (.insertAttributeAt result
                          (Attribute. (tag-gender OPT-USES-EMNLP-2014))
                          (.numAttributes result)))

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
  (let [result   (Instances. ^Instances (.determineOutputFormat this insts) 0)
        state    @(.state this)
       ;txt-ndx  (.getIndex ^SingleIndex (.getTextIndex this))          ; TODO
        icnt     (.numAttributes insts)
        ocnt     (.numAttributes result)
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
                           (if-let [^SingleIndex ndx (state opt)]
                             ;; The accumulator is a pair:
                             ;; [0] the next output column
                             ;; [1] a column-finder map {K=opt-code, V=[in-col out-col]
                             [(+ out 2) (assoc indices opt [(.getIndex ndx) out])]  ; Use option
                             [out indices]))                                        ; Unused
                         [icnt {}]
                         INDEX-OPT-KEYS)]

    (log/debug "Index options:" opts)
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
            (let [tndx   (-> (.superGetTextIndex this)
                             (.getIndex))
                  out    (weka/get-index result (tag-gender OPT-USES-EMNLP-2014))   ; FIXME: save output cols
                  [wcnt
                   hits] (check-emnlp inst tndx)
                  score  (double (reduce (fn [acc [word [hcnt weight]]]
                                           (log/fmt-debug "Score<~a>: ~6$ + (~a + ~6$)/~a"
                                                          word acc hcnt weight wcnt)
                                           (+ acc (/ (* hcnt weight)
                                                      wcnt)))
                                         0.00
                                         hits))]
              (aset ovals out score)))

          ;; Append the finished instance to the output dataset
          (.add result (DenseInstance. 1.0 ovals)))))

    ;; ...and we're done!
    result))

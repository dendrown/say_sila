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
    :methods    [[getScreenNameIndex    []       String]
                 [getFullNameIndex      []       String]
                 [getDescriptionIndex   []       String]
                 [setScreenNameIndex    [String] void]
                 [setFullNameIndex      [String] void]
                 [setDescriptionIndex   [String] void]
                 [setUpperIndices       [int]    void]]
    :exposes    {m_textIndex {:get getSuperTextIndex
                              :set setSupetTextIndex}}
    :exposes-methods {;- weka.filters.Filter ---------------------------------
                      getOptions    superGetOptions
                      setOptions    superSetOptions
                      listOptions   superListOptions
                      ;- TweetToFeatureVector --------------------------------
                      setTextIndex  superSetTextIndex}))

(set! *warn-on-reflection* true)

(def ^:const GENDER-GOLD "/srv/say_sila/weka/gender.pan2014.1-2.arff")      ; Subset!!
(def ^:const NAMES       (read-string (slurp "resources/gender/names.edn")))

(def ^:const OPT-SCREEN-NAME-NDX    "S")
(def ^:const OPT-FULL-NAME-NDX      "N")
(def ^:const OPT-DESCRIPTION-NDX    "D")
(def Options {OPT-SCREEN-NAME-NDX   (Option. "The column index that holds users' screen names."
                                             OPT-SCREEN-NAME-NDX 1 "-S <col>")

              OPT-FULL-NAME-NDX     (Option. "The column index that holds users' full names"
                                             OPT-FULL-NAME-NDX 1 "-N <col>")

              OPT-DESCRIPTION-NDX   (Option. "The column index that holds users' profile description text."
                                             OPT-DESCRIPTION-NDX 1 "-D <col>")})


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
(defn -setUpperIndices
  "
  Sets the upper index for anything column-related in the Filter state.
  "
  [^TweetToGenderFeatures this col]
  (let [tindex (.getSuperTextIndex this)]
    (.setUpper ^SingleIndex tindex col)
    (swap! (.state this) (fn [state]
                           (doseq [[_ ndx] state]
                             (.setUpper ^SingleIndex ndx col))
                           state))))



;; ---------------------------------------------------------------------------
(defn -getOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this]
  ;; Currently, all our options are SingleIndex objects
  (->> (reduce (fn [optlist [tag ^SingleIndex ndx]]
                 (let [col (.getSingleIndex ndx)
                       opt ^Option (Options tag)]
                   (conj optlist col (str "-" (.name opt)))))   ; Add in reverse order
               (seq (.superGetOptions this))
               @(.state this))
       (into-array String)))



;; ---------------------------------------------------------------------------
(defn -setOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this
   ^"[Ljava.lang.String;" opts]

  ;; CAREFUL: The calls to Weka Utils will mutate the opts array
  (let [get-opt #(not-empty (Utils/getOption ^String % opts))]

    (when-let [ndx (get-opt OPT-SCREEN-NAME-NDX)]
      (.setScreenNameIndex this ndx)))

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
(defn -determineOutputFormat
  "
  Returns a empty Instances object, which defines the filter's output after
  processing a set of input Instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             insts]
  (let [->tag  #(str "names-" %1 "-" (name %2))
        acnt   (.numAttributes insts)
        result (Instances. insts 0)]

    ;; Make sure the user stays in bounds for the attributes we hit
    (.setUpperIndices this (dec acnt))

    ;; TODO: We're doing double fields for each category.  Compare this format
    ;;       to a single field using female(pos-vals) and male(neg-vals).
    (loop [col  acnt
           opts @(.state this)]
      (when-let [[opt _] (first opts)]
        (.insertAttributeAt result (Attribute. (->tag opt :female)) col)
        (.insertAttributeAt result (Attribute. (->tag opt :male)) (inc col))
        (recur (+ col 2) (rest opts))))

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
        [_ opts] (reduce (fn [[out indices] [opt ^SingleIndex ndx]]
                           [(+ out 2)                                   ; Next outcol
                            (assoc indices opt [(.getIndex ndx) out])]) ; Tag [incol outcol]
                         [icnt {}]
                         @(.state this))]

    (log/debug "OPTIONS:" opts)
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
            (->tokens [^Instance inst a]
              (tokenize (attr-str inst a)))]

      ;; Run through all the instances...
      (doseq [^Instance inst (seq insts)]
        (let [ovals (double-array ocnt)]

          ;; Just copy in the attribute values across the input columns
          (dotimes [a icnt]
            (aset ovals a (.value inst a)))

          ;; Got F/M names jumbled in the screen name?
          (when-let [[in out] (opts OPT-SCREEN-NAME-NDX)]
            (set-counts ovals out (soc/tokenize (attr-str inst in) :lower-case)))

          ;; Got F/M names in the name or the profile description?
          (when-let [[in out] (opts OPT-FULL-NAME-NDX)]   (set-counts ovals out (->tokens inst in)))
          (when-let [[in out] (opts OPT-DESCRIPTION-NDX)] (set-counts ovals out (->tokens inst in)))

          ;; Append the finished instance to the output dataset
          (.add result (DenseInstance. 1.0 ovals)))))

    ;; ...and we're done!
    result))

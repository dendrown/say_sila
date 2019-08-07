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
                 [getDescriptionIndex   []       String]
                 [setScreenNameIndex    [String] void]
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
(def ^:const OPT-DESCRIPTION-NDX    "D")
(def Options {OPT-SCREEN-NAME-NDX   (Option. "The column index that holds users' screen names."
                                             OPT-SCREEN-NAME-NDX 1 "-S <col>")
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
  [[] (atom {OPT-SCREEN-NAME-NDX (SingleIndex.)
             OPT-DESCRIPTION-NDX (SingleIndex.)})])



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
  (set-state! this opt #(doto ^SingleIndex % (.setSingleIndex (str col)))))


;; ---------------------------------------------------------------------------
(defn -setScreenNameIndex
  "
  Sets the (one-based) column index that holds users' screen names.
  "
  [this col]
  (set-index this OPT-SCREEN-NAME-NDX col))



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
    (swap! (.state this) (partial (map (fn [[_ ^SingleIndex ndx]]
                                         (.setUpper ndx col)
                                         ndx))))))



;; ---------------------------------------------------------------------------
(defn -getOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this]
  (let [state @(.state this)]
    ;;
    ;; Currently, all our options are SingleIndex objects
    (->> (reduce #(let [ndx ^SingleIndex (state   %2)
                        opt ^Option      (Options %2)]
                    (if-let [value (not-empty (.getSingleIndex ndx))]
                      (conj %1 value (str "-" (.name opt)))         ; Add in reverse order
                      %1))                                          ; Skip unset option
                 (seq (.superGetOptions this))
                 (keys Options))

         (into-array String))))




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
  (let [->tag  #(str "gender-" %1 "-" (name %2))
        state  @(.state this)
        acnt   (.numAttributes insts)
        result (Instances. insts 0)]

    ;; Make sure the user stays in bounds for the attributes we hit
    (.setUpperIndices this (dec acnt))

    ;; TODO: We're doing double fields for each category.  Compare this format
    ;;       to a single field using female(pos-vals) and male(neg-vals).
    (loop [col  acnt
           opts (keys Options)]
      (when-let [opt (first opts)]
        (if (seq (.getSingleIndex ^SingleIndex (state opt)))
          (do (.insertAttributeAt result (Attribute. (->tag opt :female)) col)
              (.insertAttributeAt result (Attribute. (->tag opt :male)) (inc col))
              (recur (+ col 2) (rest opts)))
          (recur col (rest opts)))))

    result))



;; ---------------------------------------------------------------------------
(defn -process
  "
  Run the filter on the input instances and return the output instances.
  "
  [^TweetToGenderFeatures this
   ^Instances             insts]

  ;; NOTE: the result Instances are iteratively mutated in the Java way...
  (let [result   (Instances. ^Instances (.determineOutputFormat this insts) 0)
        state    @(.state this)
        ocnt     (.numAttributes result)
        icnt     (.numAttributes insts)
       ;txt-ndx  (.getIndex ^SingleIndex (.getTextIndex this))          ; TODO
        count-sn (fn [toks gnd]
                   (count (set/intersection toks (NAMES gnd))))]

    (doseq [^Instance inst (seq insts)]
      (let [ovals   (double-array ocnt)]

        ;; Just copy in the values across the input columns
        (dotimes [i icnt]
          (aset ovals i (.value inst i)))

        ;; Got F/M names in the screen name?
        (when-let [ndx (as-> ^SingleIndex (state OPT-SCREEN-NAME-NDX) opt
                             (and (seq (.getSingleIndex opt))
                                  (.getIndex opt)))]
          (let [toks (set (soc/tokenize (.stringValue inst (int ndx)) :upper-case))]
            (aset ovals (- ocnt 2) (double (count-sn toks :female)))
            (aset ovals (- ocnt 1) (double (count-sn toks :male)))))

        ;; Append the finished instance to the output dataset
        (.add result (DenseInstance. 1.0 ovals))))

    ;; ...and we're done!
    result))

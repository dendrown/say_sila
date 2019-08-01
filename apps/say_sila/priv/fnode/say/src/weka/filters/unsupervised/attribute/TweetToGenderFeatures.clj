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
    :methods    [[getScreenNameIndex []       String]
                 [setScreenNameIndex [String] void]
                 [setUpperIndices    [int]    void]]
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
(defonce Options {OPT-SCREEN-NAME-NDX   (Option. "The column index that holds users' screen names."
                                                 OPT-SCREEN-NAME-NDX 1 "-S <col>")})


;; ---------------------------------------------------------------------------
(defn -init
  "
  Initializes this filter's state.
  "
  []
  [[] (atom {OPT-SCREEN-NAME-NDX (SingleIndex. "1")})])



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
        acnt   (.numAttributes insts)
        acnt-1 (dec acnt)
        tndx   (.getSuperTextIndex this)
        result (Instances. insts 0)]

    ;; Make sure the user stays in bounds for the tweet text attribute
    (.setUpper ^SingleIndex tndx acnt-1)
    (.setUpperIndices this acnt-1)

    ;; TODO: We're doing double fields for each category.  Compare this format
    ;;       to a single field using female(pos-vals) and male(neg-vals).
    (loop [ndx  acnt
           cols ["screen-name"]]                    ; TODO: more to come
      (when-let [col (first cols)]
        (.insertAttributeAt result (Attribute. (->tag col :female)) ndx)
        (.insertAttributeAt result (Attribute. (->tag col :male)) (inc ndx))
        (recur (+ ndx 2) (rest cols))))

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
        sn-ndx   (.getIndex ^SingleIndex (get state OPT-SCREEN-NAME-NDX))
        count-sn (fn [toks gnd]
                   (count (set/intersection toks (NAMES gnd))))]

    (doseq [^Instance inst (seq insts)]
      (let [ovals   (double-array ocnt)
            sn-toks (set (soc/tokenize (.stringValue inst sn-ndx) :upper-case))]

        ;; Just copy in the values across the input columns
        (dotimes [i icnt]
          (aset ovals i (.value inst i)))

        ;; Got F/M names in the screen name?
        (aset ovals (- ocnt 2) (double (count-sn sn-toks :female)))
        (aset ovals (- ocnt 1) (double (count-sn sn-toks :male)))

        ;; Append the finished instance to the output dataset
        (.add result (DenseInstance. 1.0 ovals))))

    ;; ...and we're done!
    result))



;; ---------------------------------------------------------------------------
(defn -getScreenNameIndex
  "
  Returns the setting for the (one-based) column index that holds users'
  screen names.
  "
  [this]
  (let [ndx ^SingleIndex (get-state this OPT-SCREEN-NAME-NDX)]
    (.getSingleIndex ndx)))


;; ---------------------------------------------------------------------------
(defn -setScreenNameIndex
  "
  Sets the (one-based) column index that holds users' screen names.
  "
  [this col]
  (set-state! this OPT-SCREEN-NAME-NDX
                   #(doto ^SingleIndex % (.setSingleIndex (str col)))))


;; ---------------------------------------------------------------------------
(defn -setUpperIndices
  "
  Sets the upper index for anything column-related in the Filter state.
  "
  [this ndx]
  ;; TODO: We'll have more indices to handle here
  (set-state! this OPT-SCREEN-NAME-NDX
                   #(doto ^SingleIndex % (.setUpper ndx))))


;; ---------------------------------------------------------------------------
(defn -getOptions
  "
  Returns an enumeration describing the available options.
  "
  [^TweetToGenderFeatures this]
  (let [sn-opt ^Option      (Options OPT-SCREEN-NAME-NDX)
        sn-ndx ^SingleIndex (get-state this OPT-SCREEN-NAME-NDX)]
    (into-array String (conj (seq (.superGetOptions this))
                             (.getSingleIndex sn-ndx)
                             (str "-" (.name sn-opt))))))




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



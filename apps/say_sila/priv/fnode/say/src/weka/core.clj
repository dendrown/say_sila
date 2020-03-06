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
;;;; @copyright 2017-2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.core
  (:require [say.genie       :refer :all]
            [say.config      :as cfg]
            [say.genie       :as !!]
            [say.log         :as log]
            [clojure.java.io :as io]
            [clojure.string  :as str])
  (:import  [weka.core  Instance
                        Instances]
            [weka.classifiers AbstractClassifier
                              Evaluation]
            [weka.core.converters AbstractSaver
                                  ArffLoader
                                  ArffSaver
                                  CSVSaver]
            [weka.filters Filter]))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
(defprotocol Indexer
  "Facilitate getting an index from Weka entities.  This is currently a trial
  protocol, expected to grow as we go..."
  (get-index [entity ^String tag]  "Return an index by the column name (tag)"))

(extend-protocol Indexer
  Instances
  (get-index [insts ^String tag]
    (.index (.attribute insts tag))))



;;; --------------------------------------------------------------------------
(defn ^Long index1->0
  "Converts a 1-based (numeric or string) index to a 0-based long."
  [i]
  (dec (longify i)))


;;; --------------------------------------------------------------------------
(defn tag-filename
  "Turns «/path/to/filename.extn» into a map with tagged versions of the file,
  as «/path/to/filename.tag.EXTN», where EXTN is the original extension (:tagged),
  «.arff» and «.csv» to represent different output formats.

  If the caller passes a file type specification (:arff or :csv), the function
  returns a string representing the tagged filepath."
  ([fpath tag]
  (let [parts (str/split fpath #"\.")
        stub  (str/join "." (flatten [(butlast parts) (name tag)]))]
    {:tagged (str stub "." (last parts))
     :arff   (str stub ".arff")
     :csv    (str stub ".csv")}))


  ([fpath tag ftype]
  (get (tag-filename fpath tag) ftype)))


;;; --------------------------------------------------------------------------
(defn save-file
  "Writes out the given Instances to the specified ARFF or CSV file. The
  caller may specify a filetag, which will be inserted into the filename
  as «/path/to/filename.tag.extn».

  Returns the filename again as a convenience."
  ([^String    fpath
    ^Instances data
               ftype]
  ;(log/debug "FPATH:" fpath)
  (let [saver (case ftype :arff (ArffSaver.)
                          :csv  (CSVSaver.))
        fout  (io/file fpath)]
    (.createNewFile fout)
    (doto ^AbstractSaver saver
        (.setFile fout)
        (.setInstances data)
        (.writeBatch))
    fpath))


  ([fpath tag data ftype]
  (save-file (tag-filename fpath tag ftype) data ftype)))


;;; --------------------------------------------------------------------------
(defn save-results
  "Writes out the given Instances as tagged ARFF and CSV files and returns a
  filetype-keyed map with the corresponding filename values."
  [fpath tag data]
  (let [tag-fpaths (tag-filename fpath tag)]
    {:arff (save-file (:arff tag-fpaths) data :arff)
     :csv  (save-file (:csv  tag-fpaths) data :csv)}))



;;; --------------------------------------------------------------------------
(defn ^Instances filter-instances
  "Applies a filter to the specified data Instances."
  ([data sieve]
  (filter-instances data sieve []))


  ([^Instances data
    ^Filter    sieve
               opts]
  (when (not-empty opts)
    (.setOptions sieve (into-array String opts)))
  (.setInputFormat sieve data)
  (Filter/useFilter data sieve)))



;;; --------------------------------------------------------------------------
(defn ^AbstractClassifier make-learner
  "Instantiates/configures a Weka learning algorithm based on the specified tag."
  [tag]
  (let [pke #(str "weka.classifiers.functions.supportVector.PolyKernel -E " %)
        alg (first (str/split tag #"_"))
        ^AbstractClassifier
        learner (case alg
                  "lreg"    (weka.classifiers.functions.LinearRegression.)
                  "gproc"   (weka.classifiers.functions.GaussianProcesses.)
                  "smo"     (weka.classifiers.functions.SMOreg.)
                  "m5rules" (weka.classifiers.rules.M5Rules.)
                  "rforest" (weka.classifiers.trees.RandomForest.))]

    ;; Instantiate the learner and handle any non-default parameters
    (when (some #{"gproc_2" "smo_2"} [tag])
      (.setOptions learner (into-array ["-K" (pke 2)])))

    learner))



;;; --------------------------------------------------------------------------
(defn ^Instances load-arff
  "Reads in and returns the Instances from the specified ARFF file.  The caller
  may specify any number of filter keywords after the target attribute (or nil
  to specifically skip setting a class attribute)."
  ([fpath]
  (load-arff fpath nil))

  ([^String fpath
    ^String target
    &       filters]
  (let [loader (doto (ArffLoader.)
                     (.setFile (io/file fpath)))
        insts  (.getDataSet loader)]

    ; Set the target if we know what it is
    (when target
      (.setClass insts (.attribute insts target)))

    ; Apply any requested filters.
    (reduce #(filter-instances %1 %2) insts filters))))


;;; --------------------------------------------------------------------------
(defn white-box?
  "Returns true if the specified algorithm is considered a «white box model»,
   whose results are generally explainable and interpretable by a human being."
  [model]
  ;; FIXME this set is not complete!
  (contains? #{weka.classifiers.functions.LinearRegression
               weka.classifiers.functions.Logistic
              ;weka.classifiers.rules.M5Rules
               weka.classifiers.trees.J48}
             (type model)))


;;; --------------------------------------------------------------------------
(defn black-box?
  "Returns true if the specified algorithm is considered a «black box model»,
   whose results are not readily explainable. "
  [model]
  ;; I don't know anybody else...🎵
  (not (white-box? model)))


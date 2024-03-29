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
;;;; @copyright 2017-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.core
  (:require [say.genie       :refer :all]
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
(defn ^String index0->1
  "Converts a 0-based (numeric or string) index to a 1-based String."
  [i]
  (str (inc (longify i))))



;;; --------------------------------------------------------------------------
(defn ^Long index1->0
  "Converts a 1-based (numeric or string) index to a 0-based Long."
  [i]
  (dec (longify i)))



;; ---------------------------------------------------------------------------
(defn attribute-seq
  "
  Returns an sequence of the attriutes in the specified dataset.
  "
  [^Instances insts]
  (enumeration-seq (.enumerateAttributes insts)))



;; ---------------------------------------------------------------------------
(defn instance-seq
  "
  Returns an sequence of the instances in the specified dataset.
  "
  [^Instances insts]
  (enumeration-seq (.enumerateInstances insts)))


;;; --------------------------------------------------------------------------
(defn tag-filename
  "Turns «/path/to/filename.extn» into «/path/to/filename.tag.EXTN», where EXTN
  is the specified file type (ftype, which defaults to the origin extension).
  If ftype is a collection of extensions, e.g. [:arff :csv], then the function
  returns a map, keyed by these extensions, whose data elements are the tagged
  versions of fpath."
  ([fpath tag]
  (tag-filename fpath tag nil))


  ([fpath tag ftype]
  (let [parts (str/split fpath #"\.")
        stub  (str/join "." (flatten [(butlast parts) (name tag)]))
        ->tag #(str stub "." (name  %))]
    (cond
      (coll? ftype) (into {} (map #(vector % (->tag %)) ftype))
      ftype         (->tag ftype)
      :else         (->tag (last parts))))))



;;; --------------------------------------------------------------------------
(defn save-file
  "Writes out the given Instances to the specified ARFF or CSV file. The
  caller may specify a filetag, which will be inserted into the filename
  as «/path/to/filename.tag.extn».  The default file type is :arff.

  Returns the filename again as a convenience."
  ([fpath data]
  (save-file fpath data :arff))


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
  (let [tag-fpaths (tag-filename fpath tag [:arff :csv])]
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
(defn- ^Instances prep-instances
  "Intended as a utility function for load-arff and load-data.  This function
  sets the target class, if specified, and performs data filtering."
  [^Instances insts
              target
              filters]
    ;; Set the target if we know what it is
    (when target
      (.setClass insts (.attribute insts (name target))))

    ; Apply any requested filters.
    (reduce #(filter-instances %1 %2) insts filters))



;;; --------------------------------------------------------------------------
(defn ^Instances load-arff
  "Reads in and returns the Instances from the specified ARFF file.  The caller
  may specify any number of filter keywords after the target attribute (or nil
  to specifically skip setting a class attribute)."
  ([fpath]
  (load-arff fpath nil))

  ([fpath target & filters]
  ;; Perform Weka ARFF load and prepare Instances
  (-> (doto (ArffLoader.)
            (.setFile (io/file fpath)))
      (.getDataSet)
      (prep-instances target filters))))



;;; --------------------------------------------------------------------------
(defn ^Instances load-dataset
  "Accepts either an ARFF filepath or a preloaded set of Instances.  The
  function prepares the dataset by setting the target class and applying
  the specified filters if any."
  ([fpath]
  (load-dataset fpath nil))


  ([data target & filters]
  (if (string? data)
      (apply load-arff data target filters)
      (prep-instances  data target filters))))



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


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
;;;; @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns sila-weka.weka
  (:require [clojure.java.io :as io]
            [clojure.string  :as str])
  (:import  (weka.core Instances)
            (weka.filters Filter)
            (weka.core.converters ArffLoader ArffSaver)
            (weka.filters.unsupervised.attribute TweetToEmbeddingsFeatureVector)))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defn load-arff
  "
  Reads in and returns the Instances from the specified ARFF file
  "
  [#^String fpath]
  (let [loader (ArffLoader.)]
    ; Pull the instances and give 'em to the caller
    (.setFile loader (io/file fpath))
    (.getDataSet loader)))


;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defn save-arff
  "
  Writes out the given Instances to the specified ARFF file
  "
  [#^String    fpath
   #^Instances data]
  (let [saver (ArffSaver.)
        fout  (io/file fpath)]
    (.createNewFile fout)
    (.setFile saver fout)
    (.setInstances saver data)
    (.writeBatch saver)))


;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defn tag-filename
  "
  Turns «/path/to/filename.extn» into «/path/to/filename.tag.extn»
  "
  [fpath tag]
  (let [parts (str/split fpath #"\.")]
    (str/join "." (flatten [(butlast parts) tag (last parts)]))))


;;; --------------------------------------------------------------------------
;;; ┏━╸┏━╸╻ ╻┏━┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┣╸ ┃  ┣━┫┃ ┃╺━╸┣━┫┣┳┛┣╸ ┣╸ 
;;; ┗━╸┗━╸╹ ╹┗━┛   ╹ ╹╹┗╸╹  ╹ 
;;; --------------------------------------------------------------------------
(defn echo-arff
  "
  This is a test function.  It reads in an ARFF file:

    «/path/to/sample.arff»
    
  and creates an «echo» copy
  
    «/path/to/sample.echo.arff»
  "
  [fpath]
  (let [data (load-arff fpath)]
    (println "Echoing " fpath)
    (save-arff (tag-filename fpath "echo") data)))



;;; --------------------------------------------------------------------------
;;; --------------------------------------------------------------------------
(defn embed-arff
  "
  Reads in an ARFF file with tweets and writes it back out with embeddings.
  "
  [fpath]
    (let [data        (load-arff fpath)
          filter      (TweetToEmbeddingsFeatureVector.)]
      (println "Embedding " fpath)
      ; Default embeddings: "w2v.twitter.edinburgh.100d.csv.gz"
      (.setOptions filter (into-array String ["-I" "3"  ; Text attribute
                                              "-S" "0"  ; 0=avg, 1=add, 2=cat
                                              "-K" "15" ; Concat word count
                                              "-L"      ; Lowercase
                                              "-O"]))   ; Normalize URLs/@users
      (.setInputFormat filter data)
      (save-arff (tag-filename fpath "embed")
                 (Filter/useFilter data filter))))

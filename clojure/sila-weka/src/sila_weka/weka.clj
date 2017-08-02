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
  (:require [clojure.string :as str])
  (:import  (java.io File)
            (weka.core.converters ArffLoader ArffSaver)))

(set! *warn-on-reflection* true)


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
  [orig-fpath]
  ; Bring 'em in
  (let [loader    (ArffLoader.)
        orig-file (File. #^String orig-fpath)]
    (.setFile loader orig-file)
    ; Pull the instances and save them back out
    (let [data       (.getDataSet loader)
          saver      (ArffSaver.)
          orig-parts (str/split orig-fpath #"\.")
          echo-fpath (str/join "." (flatten [(butlast orig-parts) "echo" (last orig-parts)]))
          echo-file  (File. echo-fpath)]
      (println "Echoing to " echo-fpath)
      (.createNewFile echo-file)
      (.setFile saver echo-file)
      (.setInstances saver data)
      (.writeBatch saver))))





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
            (weka.core.converters ArffLoader
                                  ArffSaver)
            (weka.filters.unsupervised.attribute TweetToEmbeddingsFeatureVector
                                                 TweetToLexiconFeatureVector
                                                 TweetToSentiStrengthFeatureVector)))

(set! *warn-on-reflection* true)

(def ^:const +ARFF-TEXT-ATTR+ "3")
(def ^:const +FILTERS+ {:embed  {:filter  '(TweetToEmbeddingsFeatureVector.)
                                 :options ["-I" +ARFF-TEXT-ATTR+
                                           "-S" "0"    ; 0=avg, 1=add, 2=cat
                                           "-K" "15"   ; Concat word count
                                           "-L"        ; Lowercase
                                           "-O"]}      ; Normalize URLs/@users
                                                       ; Default embeddings [ -B ]:
                                                       ;   w2v.twitter.edinburgh.100d.csv.gz
                        :lex    {:filter  '(TweetToLexiconFeatureVector.)
                                 :options ["-I" +ARFF-TEXT-ATTR+
                                           "-A"        ; MPQA Lexicon
                                           "-D"        ; Bing Liu
                                           "-F"        ; AFINN
                                           "-H"        ; S140
                                           "-J"        ; NRC-Hash-Sent
                                           "-L"        ; NRC-10 Emotion
                                           "-N"        ; NRC-10-Expanded Emotion
                                           "-P"        ; NRC Hashtag Emotion
                                           "-Q"        ; SentiWordNet
                                           "-R"        ; Emoticon List
                                           "-T"        ; Negation List
                                           "-U"        ; Lowercase (not upper)
                                           "-O"]}      ; Normalize URLs/@users
                        :senti  {:filter  '(TweetToLexiconFeatureVector.)
                                 :options ["-I" +ARFF-TEXT-ATTR+
                                           "-U"        ; Lowercase (not upper)
                                           "-O"]}})    ; Normalize URLs/@users


;;; --------------------------------------------------------------------------
;;; ╻  ┏━┓┏━┓╺┳┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┃  ┃ ┃┣━┫ ┃┃╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━╸┗━┛╹ ╹╺┻┛   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn load-arff
  "
  Reads in and returns the Instances from the specified ARFF file
  "
  [fpath]
  (let [loader (ArffLoader.)]
    ; Pull the instances and give 'em to the caller
    (.setFile loader (io/file fpath))
    (.getDataSet loader)))



;;; --------------------------------------------------------------------------
;;; ┏━┓┏━┓╻ ╻┏━╸   ┏━┓┏━┓┏━╸┏━╸
;;; ┗━┓┣━┫┃┏┛┣╸ ╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━┛╹ ╹┗┛ ┗━╸   ╹ ╹╹┗╸╹  ╹
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
    (doto saver
        (.setFile fout)
        (.setInstances data)
        (.writeBatch))
    (.numInstances data)))


;;; --------------------------------------------------------------------------
;;; ╺┳╸┏━┓┏━╸   ┏━╸╻╻  ┏━╸┏┓╻┏━┓┏┳┓┏━╸
;;;  ┃ ┣━┫┃╺┓╺━╸┣╸ ┃┃  ┣╸ ┃┗┫┣━┫┃┃┃┣╸
;;;  ╹ ╹ ╹┗━┛   ╹  ╹┗━╸┗━╸╹ ╹╹ ╹╹ ╹┗━╸
;;; --------------------------------------------------------------------------
(defn tag-filename
  "
  Turns «/path/to/filename.extn» into «/path/to/filename.tag.extn»
  "
  [fpath tag]
  (let [parts (str/split fpath #"\.")]
    (str/join "." (flatten [(butlast parts) tag (last parts)]))))



;;; --------------------------------------------------------------------------
;;; ┏━╸╻╻  ╺┳╸┏━╸┏━┓   ╻┏┓╻┏━┓╺┳╸┏━┓┏┓╻┏━╸┏━╸┏━┓
;;; ┣╸ ┃┃   ┃ ┣╸ ┣┳┛╺━╸┃┃┗┫┗━┓ ┃ ┣━┫┃┗┫┃  ┣╸ ┗━┓
;;; ╹  ╹┗━╸ ╹ ┗━╸╹┗╸   ╹╹ ╹┗━┛ ╹ ╹ ╹╹ ╹┗━╸┗━╸┗━┛
;;; --------------------------------------------------------------------------
(defn #^Instances filter-instances
  "
  Applies a filter to the specified data Instances
  "
  [data flt-key]
  (let [flt-map (flt-key  +FILTERS+)
        opts    (:options flt-map)
        sieve   #^Filter (eval (:filter  flt-map))]
  (doto sieve
    (.setOptions     (into-array String opts))
    (.setInputFormat data))
  (Filter/useFilter data sieve)))



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
;;; ┏━╸╻╻  ╺┳╸┏━╸┏━┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┣╸ ┃┃   ┃ ┣╸ ┣┳┛╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ╹  ╹┗━╸ ╹ ┗━╸╹┗╸   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn filter-arff
  "
  Reads in an ARFF file with tweets and writes it back out after applying
  the specified filter.
  "
  [fpath flt-key]
    (let [data  (load-arff fpath)
          tag   (name flt-key)]
      (println "Filtering<" tag ">: " fpath)
      (save-arff (tag-filename fpath tag)
                 (filter-instances data flt-key))))




;;; --------------------------------------------------------------------------
;;; ┏━╸┏┳┓┏┓ ┏━╸╺┳┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┣╸ ┃┃┃┣┻┓┣╸  ┃┃╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━╸╹ ╹┗━┛┗━╸╺┻┛   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn embed-arff
  "
  Reads in an ARFF file with tweets and writes it back out with embeddings.
  "
  [fpath]
  (filter-arff fpath :embed))



;;; --------------------------------------------------------------------------
;;; ╻  ┏━╸╻ ╻╻┏━╸╻ ╻   ┏━┓┏━┓┏━╸┏━╸
;;; ┃  ┣╸ ┏╋┛┃┣╸ ┗┳┛╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━╸┗━╸╹ ╹╹╹   ╹    ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn lexify-arff
  "
  Reads in an ARFF file with tweets and writes it back out with lexicon feature
  vectors.
  "
  [fpath]
  (filter-arff fpath :lex))



;;; --------------------------------------------------------------------------
;;; ┏━┓┏━╸┏┓╻╺┳╸╻   ┏━┓┏━┓┏━╸┏━╸
;;; ┗━┓┣╸ ┃┗┫ ┃ ┃╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━┛┗━╸╹ ╹ ╹ ╹   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn senti-arff
  "
  Reads in an ARFF file with tweets and writes it back out with SentiStrength
  feature vectors.
  "
  [fpath]
  (filter-arff fpath :senti))

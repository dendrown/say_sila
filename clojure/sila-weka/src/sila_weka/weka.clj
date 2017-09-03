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
            [clojure.string  :as str]
            [sila-weka.log   :as log])
  (:import  [weka.core Instances]
            [weka.filters Filter]
            [weka.core.converters AbstractSaver
                                  ArffLoader
                                  ArffSaver
                                  CSVSaver]
            [weka.filters.unsupervised.attribute TweetToEmbeddingsFeatureVector
                                                 TweetToInputLexiconFeatureVector
                                                 TweetToLexiconFeatureVector
                                                 TweetToSentiStrengthFeatureVector]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; LEXI-NOTES:
;; :bws  BWS         [4e] NRC-AffectIntensity-Lexicon.arff
;; :lex  Hash-Lex -P [8e] NRC-Hashtag-Emotion-Lexicon-v0.2
;;       Emo-Lex  -L [8e] NRC-emotion-lexicon-wordlevel-v0.92
;;       Expanded -N [8e] w2v-dp-BCC-Lex
;;
;; NOTE: Even though the AffectiveTweets Filter classes are imported in this
;;       namespace, we must fully qualify the :filter elements in +FILTERS+
;;       because the quoted code is likely to be evaluated in a non-binding-thread
;;       (future) launched from another namespace, which won't have access to the
;;       Java imports.
;; ---------------------------------------------------------------------------
(def ^:const +ARFF-TEXT-ATTR+ "3")
(def ^:const +FILTERS+ {:embed  {:filter  '(weka.filters.unsupervised.attribute.TweetToEmbeddingsFeatureVector.)
                                 :options ["-I" +ARFF-TEXT-ATTR+
                                           "-S" "0"    ; 0=avg, 1=add, 2=cat
                                           "-K" "15"   ; Concat word count
                                           "-L"        ; Lowercase
                                           "-O"]}      ; Normalize URLs/@users
                                                       ; Default embeddings [ -B ]:
                                                       ;   w2v.twitter.edinburgh.100d.csv.gz
                        :bws    {:filter  '(weka.filters.unsupervised.attribute.TweetToInputLexiconFeatureVector.)
                                 :options ["-I" +ARFF-TEXT-ATTR+
                                           "-U"        ; Lowercase (not upper)
                                           "-O"]}      ; Normalize URLs/@users
                        :lex    {:filter  '(weka.filters.unsupervised.attribute.TweetToLexiconFeatureVector.)
                                 :options ["-I" +ARFF-TEXT-ATTR+
                                           "-A"        ; MPQA Lexicon
                                           "-D"        ; Bing Liu
                                           "-F"        ; AFINN
                                           "-H"        ; S140
                                           "-J"        ; NRC-Hash-Sent
                                           "-L"        ; NRC-10 Emotion [EmoLex]
                                           "-N"        ; NRC-10-Expanded Emotion
                                           "-P"        ; NRC Hashtag Emotion
                                           "-Q"        ; SentiWordNet
                                           "-R"        ; Emoticon List
                                           "-T"        ; Negation List
                                           "-U"        ; Lowercase (not upper)
                                           "-O"]}      ; Normalize URLs/@users
                        :senti  {:filter  '(weka.filters.unsupervised.attribute.TweetToSentiStrengthFeatureVector.)
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
;;; ┏━┓┏━┓╻ ╻┏━╸   ┏━╸╻╻  ┏━╸
;;; ┗━┓┣━┫┃┏┛┣╸ ╺━╸┣╸ ┃┃  ┣╸
;;; ┗━┛╹ ╹┗┛ ┗━╸   ╹  ╹┗━╸┗━╸
;;; --------------------------------------------------------------------------
(defn save-file
  "
  Writes out the given Instances to the specified ARFF or CSV file.

  Returns the filename again as a convenience.
  "
  [^String    fpath
   ^Instances data
               ftype]
  (let [saver (case ftype :arff (ArffSaver.)
                          :csv  (CSVSaver.))
        fout  (io/file fpath)]
    (.createNewFile fout)
    (doto ^AbstractSaver saver
        (.setFile fout)
        (.setInstances data)
        (.writeBatch))
    fpath))


;;; --------------------------------------------------------------------------
;;; ╺┳╸┏━┓┏━╸   ┏━╸╻╻  ┏━╸┏┓╻┏━┓┏┳┓┏━╸
;;;  ┃ ┣━┫┃╺┓╺━╸┣╸ ┃┃  ┣╸ ┃┗┫┣━┫┃┃┃┣╸
;;;  ╹ ╹ ╹┗━┛   ╹  ╹┗━╸┗━╸╹ ╹╹ ╹╹ ╹┗━╸
;;; --------------------------------------------------------------------------
(defn tag-filename
  "
  Turns «/path/to/filename.extn» into a map with tagged versions of the file,
  as «/path/to/filename.tag.EXTN», where EXTN is the original extension (:tagged),
  «.arff» and «.csv» to represent different output formats.
  "
  [fpath tag]
  (let [parts (str/split fpath #"\.")
        stub  (str/join "." (flatten [(butlast parts) tag]))]
    {:tagged (str stub "." (last parts))
     :arff   (str stub ".arff")
     :csv    (str stub ".csv")}))



;;; --------------------------------------------------------------------------
;;; ┏━╸╻╻  ╺┳╸┏━╸┏━┓   ╻┏┓╻┏━┓╺┳╸┏━┓┏┓╻┏━╸┏━╸┏━┓
;;; ┣╸ ┃┃   ┃ ┣╸ ┣┳┛╺━╸┃┃┗┫┗━┓ ┃ ┣━┫┃┗┫┃  ┣╸ ┗━┓
;;; ╹  ╹┗━╸ ╹ ┗━╸╹┗╸   ╹╹ ╹┗━┛ ╹ ╹ ╹╹ ╹┗━╸┗━╸┗━┛
;;; --------------------------------------------------------------------------
(defn ^Instances filter-instances
  "
  Applies a filter to the specified data Instances
  "
  [data flt-key]
  (let [flt-map (flt-key  +FILTERS+)
        opts    (:options flt-map)
        sieve   ^Filter (eval (:filter  flt-map))]
  (doto sieve
    (.setOptions     (into-array String opts))
    (.setInputFormat data))
  (Filter/useFilter data sieve)))



;;; --------------------------------------------------------------------------
;;; ┏━╸╻╻  ╺┳╸┏━╸┏━┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┣╸ ┃┃   ┃ ┣╸ ┣┳┛╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ╹  ╹┗━╸ ╹ ┗━╸╹┗╸   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn filter-arff
  "
  Reads in an ARFF file with tweets and writes it back out after applying
  the specified filter.

  Returns a vector of the output filenames.
  "
  [fpath flt-key]
    (let [data-in   (load-arff fpath)
          data-out  (filter-instances data-in flt-key)
          tag        (name flt-key)
          tag-fpaths (tag-filename fpath tag)]
      (log/debug "Filter<" tag ">: " fpath)
      {:arff (save-file (:arff tag-fpaths) data-out :arff)
       :csv  (save-file (:csv  tag-fpaths) data-out :csv)}))




;;; --------------------------------------------------------------------------
;;; ┏━╸┏┳┓┏━┓╺┳╸┏━╸   ┏━┓┏━┓┏━╸┏━╸
;;; ┣╸ ┃┃┃┃ ┃ ┃ ┣╸ ╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━╸╹ ╹┗━┛ ╹ ┗━╸   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn emote-arff
	"
	Reads in an ARFF file with tweets, applies embedding/sentiment/emotion filters,
	as needed for «Say Sila», and then outputs the results in ARFF and CSV formats.

  NOTE: This will be the main point of definition of what (Erlang) Sila wants,
        until such time as it starts sending us its specific configurations.
	"
  [fpath]
  (log/debug "emote lexicon:" TweetToInputLexiconFeatureVector/NRC_AFFECT_INTENSITY_FILE_NAME)
  (filter-arff fpath :bws))



;;; --------------------------------------------------------------------------
;;; ┏━╸┏┳┓┏┓ ┏━╸╺┳┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┣╸ ┃┃┃┣┻┓┣╸  ┃┃╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━╸╹ ╹┗━┛┗━╸╺┻┛   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn embed-arff
  "
  Reads in an ARFF file with tweets and writes it back out with embeddings.

  WARNING: This is a test wrapper, and will be deleted soon; use filter-arff
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

  WARNING: This is a test wrapper, and will be deleted soon; use filter-arff
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

  WARNING: This is a test wrapper, and will be deleted soon; use filter-arff
  "
  [fpath]
  (filter-arff fpath :senti))


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
            [sila-weka.genie :as genie]
            [sila-weka.log   :as log])
  (:import  [weka.core  Instance
                        Instances]
            [weka.filters Filter]
            [weka.core.converters AbstractSaver
                                  ArffLoader
                                  ArffSaver
                                  CSVSaver]
            [weka.filters.unsupervised.attribute Reorder
                                                 TweetToEmbeddingsFeatureVector
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
(def ^:const +ARFF-TEXT-ATTR-NUM+ 3)
(def ^:const +ARFF-TEXT-ATTR+    (str +ARFF-TEXT-ATTR-NUM+))

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
                                           "-O"]}      ; Normalize URLs/@users
                        :attrs  {:filter  '(weka.filters.unsupervised.attribute.Reorder.)
                                 :options [; Remove text attribute from the output
                                           "-R" (str "1-" (dec +ARFF-TEXT-ATTR-NUM+)
                                                     ","  (inc +ARFF-TEXT-ATTR-NUM+) "-last")]}})


;;; --------------------------------------------------------------------------
;;; ╻  ┏━┓┏━┓╺┳┓   ┏━┓┏━┓┏━╸┏━╸
;;; ┃  ┃ ┃┣━┫ ┃┃╺━╸┣━┫┣┳┛┣╸ ┣╸
;;; ┗━╸┗━┛╹ ╹╺┻┛   ╹ ╹╹┗╸╹  ╹
;;; --------------------------------------------------------------------------
(defn ^Instances load-arff
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
  [^Instances data flt-key]
  (let [flt-map (flt-key  +FILTERS+)
        opts    (:options flt-map)
        sieve   ^Filter (eval (:filter  flt-map))
        tag     (name flt-key)]
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
  Reads in an ARFF file with tweets and writes it back out after applying:
   (1) the specified list of filters
   (2) the Reorder filter to remove the tweet text

  Returns a vector of the output filenames.
  "
  [fpath flt-keys]
  (let [filters    (genie/listify flt-keys)
        data-in    (load-arff fpath)
        data-mid   (reduce #(filter-instances %1 %2) data-in filters)   ; Apply filter(s)
        data-out   (filter-instances data-mid :attrs)                   ; Remove text attr
        tag        (str/join "." (map #(str (name %)) filters))
        tag-fpaths (tag-filename fpath tag)]
    (log/debug "Filter<" tag ">:" (.numAttributes data-in)  "x" (.size data-in)
               "==>"              (.numAttributes data-out) "x" (.size data-out))
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
  ;(log/debug "emote lexicon:" TweetToInputLexiconFeatureVector/NRC_AFFECT_INTENSITY_FILE_NAME)
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


;;; --------------------------------------------------------------------------
(defn prepare-ml
  "
  This is (for the moment) an ad-hoc function for use in DIC-9305.
  "
  [^String fpath
   ^String value]

  (let [data-in     (load-arff fpath)
        tag-fpaths  (tag-filename fpath "ML")
        adder       (doto (weka.filters.unsupervised.attribute.Add.)
                          (.setAttributeIndex "first")
                          (.setNominalLabels  "REG,BIG")
                          (.setAttributeName  "player")
                          (.setInputFormat    data-in))
        data-out    (Filter/useFilter data-in adder)]
    (doseq [d data-out] (.setValue ^Instance d 0 value))

    (.setClassIndex data-out 0)
    (save-file (:arff tag-fpaths) data-out :arff)))


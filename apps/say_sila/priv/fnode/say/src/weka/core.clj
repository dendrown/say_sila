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
  (:require [say.config      :as cfg]
            [say.genie       :as !!]
            [say.log         :as log]
            [clojure.java.io :as io]
            [clojure.string  :as str])
  (:import  [affective.core ArffLexiconEvaluator]
            [java.util  Random]
            [weka.core  Instance
                        Instances]
            [weka.classifiers Evaluation]
            [weka.classifiers.functions LinearRegression]
            [weka.core.converters AbstractSaver
                                  ArffLoader
                                  ArffSaver
                                  CSVSaver]
            [weka.core.stemmers SnowballStemmer]
            [weka.core.stopwords WordsFromFile]
            [weka.filters Filter]
            [weka.filters.unsupervised.attribute RemoveByName
                                                 Reorder
                                                 TweetToEmbeddingsFeatureVector
                                                ;TweetToFeatureVector
                                                 TweetToInputLexiconFeatureVector
                                                 TweetToLexiconFeatureVector
                                                 TweetToSentiStrengthFeatureVector]))

(set! *warn-on-reflection* true)

(def ^:const RNG-SEED    1)                             ; Weka's default random seed
(def ^:const CV-FOLDS    10)                            ; Folds for cross-validation evaluation
(def ^:const ACK         {:status :ack})                ; Positive acknowledgement response
(def ^:const NAK         {:status :nak})                ; Negative acknowledgement response
(def ^:const STOPLIST-EN "resources/stoplist_en.txt")   ; Lucene 7.2.1 ENGLISH_STOP_WORDS_SET


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

(def ^:const +EMOTE-FILTER+ :bws)


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
(defn save-file
  "Writes out the given Instances to the specified ARFF or CSV file.

  Returns the filename again as a convenience."
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
(defn tag-filename
  "Turns «/path/to/filename.extn» into a map with tagged versions of the file,
  as «/path/to/filename.tag.EXTN», where EXTN is the original extension (:tagged),
  «.arff» and «.csv» to represent different output formats."
  [fpath tag]
  (let [parts (str/split fpath #"\.")
        stub  (str/join "." (flatten [(butlast parts) tag]))]
    {:tagged (str stub "." (last parts))
     :arff   (str stub ".arff")
     :csv    (str stub ".csv")}))



;;; --------------------------------------------------------------------------
(defn- save-results
  "Writes out the given Instances as tagged ARFF and CSV files and returns a
  filetype-keyed map with the corresponding filename values."
  [fpath tag data]
  (let [tag-fpaths (tag-filename fpath tag)]
    {:arff (save-file (:arff tag-fpaths) data :arff)
     :csv  (save-file (:csv  tag-fpaths) data :csv)}))



;;; --------------------------------------------------------------------------
(defn ^Instances filter-instances
  "Applies a filter to the specified data Instances."
  ([data flt-key]
  (let [flt-map (flt-key  +FILTERS+)
        sieve   (eval (:filter  flt-map))
        opts    (:options flt-map)]
    (filter-instances data sieve opts)))


  ([^Instances data
    ^Filter    sieve
               opts]
  (doto sieve
        (.setOptions (into-array String opts))
        (.setInputFormat data))
  (Filter/useFilter data sieve)))



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
(defn filter-arff
  "Reads in an ARFF file with tweets and writes it back out after applying:
   (1) the specified list of filters
   (2) the Reorder filter to remove the tweet text

  Returns a vector of the output filenames."
  [fpath flt-keys]
  (let [filters    (!!/listify flt-keys)
        data-in    (load-arff fpath)
        data-mid   (reduce #(filter-instances %1 %2) data-in filters)   ; Apply filter(s)
        data-out   (filter-instances data-mid :attrs)                   ; Remove text attr
        tag        (str/join "." (map #(str (name %)) filters))]
    (log/debug "Filter<" tag ">:" (.numAttributes data-in)  "x" (.size data-in)
               "==>"              (.numAttributes data-out) "x" (.size data-out))
    (save-results fpath tag data-out)))



;;; --------------------------------------------------------------------------
(defn regress
  "Runs a Linear Regression model on the ARFF at the specified filepath."
  [{arff      :arff
   target     :target
   excl-attrs :exclude
   work-csv   :work_csv}]

  (log/info "Excluding attributes:" excl-attrs)
  (try
    (let [insts0 (load-arff arff target)
          insts  (if excl-attrs
                     (filter-instances insts0 (RemoveByName.) ["-E" excl-attrs])
                     insts0)]

      ;; Since Weka leaves out some statistical measures, make a work CSV for Incanter
      (when work-csv
        (save-file work-csv insts :csv))

      ;; If they didn't send a target, select the last attribute
      (when-not target
        (.setClassIndex insts (dec (.numAttributes insts))))

      ;; Use N-fold cross validation for training/evaluation
      (let [model   (LinearRegression.)
            audit   (doto (Evaluation. insts)
                          (.crossValidateModel model insts CV-FOLDS (Random. RNG-SEED)))
            summary (.toSummaryString audit)]

      (letfn [(attr-coeff [[ndx coeff]]
                (let [attr  (.attribute insts (int ndx))
                      tag   (.name attr)]
                  [tag coeff]))]

          ;; Results are obtained from CV folds, but
          ;; final model training uses the full dataset
          (.buildClassifier model insts)
          (log/info "Model:\n" (str model))
          (log/info "Summary:\n" summary)
          ;; Prepare a response for the caller
          ;;
          ;; NOTE: LinearRegression.numParameters() does not include unused params!
          ;;       The Weka LR coefficient array appears to take have the form:
          ;;       [ a0, a1, ..., aN-1, 0.0, intercept ]
          ;;
          ;;       A better way may be to use the "-C" option, so the LR algorithm
          ;;       does not try to eliminate colinear attributes.
          ;;
          ;; TODO: As Weka documentation seems to be lacking here, I should take the
          ;;       time to verify the array structure in the Weka source code.
          (let [;param-cnt   (.numParameters model)
                coefficients (.coefficients model)
                coeff-cnt    (- (count coefficients) 2)                             ; See note above
                intercept    (last coefficients)]
            (assoc ACK :model        (type model)
                       :instances    (.numInstances insts)
                       :correlation  (.correlationCoefficient audit)
                       :intercept    intercept
                       :coefficients (into {} (map attr-coeff                       ; ZIP:
                                                   (map vector (range coeff-cnt)    ; Attr-indexes
                                                               coefficients)))))))) ; Coeff-values
    (catch Exception ex
           (log/fail ex "Linear regression failed")
           (assoc NAK :info  (.getMessage ex)))))



;;; --------------------------------------------------------------------------
(defn emote-arff
  "Reads in an ARFF file with tweets, applies embedding/sentiment/emotion filters,
  as needed for «Say Sila», and then outputs the results in ARFF and CSV formats.

  NOTE: This will be the main point of definition of what (Erlang) Sila wants,
        until such time as it starts sending us its specific configurations."
  [fpath]
  (log/debug "Emoting:" fpath)
  (letfn [(make-emoter [opt]
            (let [emoter (TweetToInputLexiconFeatureVector.)]
              ; Our only defined NLP option is English-Stoplist-Porter
              (when (= opt :english)
                (let [stoplist (doto (WordsFromFile.)
                                     (.setStopwords (io/file STOPLIST-EN)))
                      lexer    (doto (ArffLexiconEvaluator.)
                                     (.setStemmer (SnowballStemmer. "english")))]
                  (doto emoter
                        (.setLexiconEval (into-array ^ArffLexiconEvaluator [lexer]))
                        (.setStopwordsHandler stoplist)
                        (.setStemmer (SnowballStemmer. "english")))))
              emoter))]

    (let [emoter    (make-emoter (cfg/?? :emote :nlp))
          data-in   (load-arff fpath)
          data-mid  (filter-instances data-in  emoter (:options (:bws +FILTERS+)))
          data-out  (filter-instances data-mid :attrs)]

      (log/debug "emote lexicon:" TweetToInputLexiconFeatureVector/NRC_AFFECT_INTENSITY_FILE_NAME)
      (save-results fpath "emote" data-out))))



;;; --------------------------------------------------------------------------
(defn embed-arff
  "Reads in an ARFF file with tweets and writes it back out with embeddings.

  WARNING: This is a test wrapper, and will be deleted soon; use filter-arff."
  [fpath]
  (filter-arff fpath :embed))



;;; --------------------------------------------------------------------------
(defn lexify-arff
  "Reads in an ARFF file with tweets and writes it back out with lexicon feature
  vectors.

  WARNING: This is a test wrapper, and will be deleted soon; use filter-arff."
  [fpath]
  (filter-arff fpath :lex))



;;; --------------------------------------------------------------------------
(defn senti-arff
  "Reads in an ARFF file with tweets and writes it back out with SentiStrength
  feature vectors.

  WARNING: This is a test wrapper, and will be deleted soon; use filter-arff."
  [fpath]
  (filter-arff fpath :senti))



;;; --------------------------------------------------------------------------
(defn prepare-ml
  "This is (for the moment) an ad-hoc function for use in DIC9315."
  [^String big-fpath
   ^String reg-fpath]

  (let [big-data    (load-arff big-fpath)
        reg-data    (load-arff reg-fpath)

        adder       (doto (weka.filters.unsupervised.attribute.Add.)    ; Define filter
                          (.setAttributeIndex "first")
                          (.setNominalLabels  "REG,BIG")
                          (.setAttributeName  "player")
                          (.setInputFormat    big-data))

        convert     (fn [^Instances data                                ; Filter & populate
                         ^String    value]
                      (let [fdata (Filter/useFilter data adder)]
                        (doseq [inst fdata] (.setValue ^Instance inst 0 value))
                        (.setClassIndex fdata 0)
                        fdata))

        data-out    (doto ^Instances (convert big-data "BIG")
                                     (.addAll ^Instances (convert reg-data "REG"))
                                     (.randomize !!/RNG))]

    {:arff (save-file "/tmp/dic9315.ML.arff" data-out :arff)
     :csv  (save-file "/tmp/dic9315.ML.csv"  data-out :csv)}))


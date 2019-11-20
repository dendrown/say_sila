;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion and Tweet functionality for Weka
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.tweet
  (:require [say.config      :as cfg]
            [say.genie       :as !!]
            [say.log         :as log]
            [weka.core       :as weka]
            [clojure.java.io :as io]
            [clojure.string  :as str])
  (:import  [affective.core ArffLexiconEvaluator]
            [java.util  Random]
            [weka.core  Instance
                        Instances]
            [weka.classifiers Evaluation]
            [weka.classifiers.functions LinearRegression]
            [weka.core.stemmers SnowballStemmer]
            [weka.core.stopwords WordsFromFile]
            [weka.filters Filter]
            [weka.filters.unsupervised.attribute RemoveByName
                                                 Reorder
                                                 ;; Affective Tweets library
                                                 TweetToFeatureVector
                                                 TweetToEmbeddingsFeatureVector
                                                 TweetToInputLexiconFeatureVector
                                                 TweetToLexiconFeatureVector
                                                 TweetToSentiStrengthFeatureVector
                                                 ;; Say-Sila extensions
                                                 TweetToGenderFeatures]))
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
;;       namespace, we must fully qualify the :filter elements in FILTERS
;;       because the quoted code is likely to be evaluated in a non-binding-thread
;;       (future) launched from another namespace, which won't have access to the
;;       Java imports.
;; ---------------------------------------------------------------------------
(def ^:const ATTRS-IN   [:id :lang :screen_name :name :description :text])
(def ^:const ATTRS-OUT  (take 3 ATTRS-IN))
(def ^:const ARFF-ATTRS (reduce (fn [acc [col ndx]] (assoc acc col ndx))
                                {}
                                (map vector ATTRS-IN
                                            (range 1 (inc (count ATTRS-IN))))))
(def ^:const TEXT-ATTR  (str (:text ARFF-ATTRS)))

(def ^:const FILTERS    {:embed {:filter  '(weka.filters.unsupervised.attribute.TweetToEmbeddingsFeatureVector.)
                                 :options ["-I" TEXT-ATTR
                                           "-S" "0"    ; 0=avg, 1=add, 2=cat
                                           "-K" "15"   ; Concat word count
                                           "-L"        ; Lowercase
                                           "-O"]}      ; Normalize URLs/@users
                                                       ; Default embeddings [ -B ]:
                                                       ;   w2v.twitter.edinburgh.100d.csv.gz
                        :bws    {:filter  '(weka.filters.unsupervised.attribute.TweetToInputLexiconFeatureVector.)
                                 :options ["-I" TEXT-ATTR
                                           "-U"        ; Lowercase (not upper)
                                           "-O"]}      ; Normalize URLs/@users
                        :lex    {:filter  '(weka.filters.unsupervised.attribute.TweetToLexiconFeatureVector.)
                                 :options ["-I" TEXT-ATTR
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
                                 :options ["-I" TEXT-ATTR
                                           "-U"        ; Lowercase (not upper)
                                           "-O"]}      ; Normalize URLs/@users
                        :attrs  {:filter  '(weka.filters.unsupervised.attribute.Reorder.)
                                 :options [; Remove full name/description/text attributes from the output
                                           "-R" (str "1-" (count ATTRS-OUT)
                                                     ","  (inc (count ATTRS-IN)) "-last")]}})



;;; --------------------------------------------------------------------------
(defn ^Instances filter-instances
  "Applies a pre-defined filter to the specified data Instances.  The arity/3
  version acts as a pass-through to weka.core's function of the same name."
  ([data flt-key]
  (let [flt-map (flt-key  FILTERS)
        sieve   (eval (:filter  flt-map))
        opts    (:options flt-map)]
    (filter-instances data sieve opts)))


  ([data sieve opts]
  (weka/filter-instances data sieve opts)))


;;; --------------------------------------------------------------------------
(defn filter-arff
  "Reads in an ARFF file with tweets and writes it back out after applying:
   (1) the specified list of filters
   (2) the Reorder filter to remove the tweet text

  Returns a vector of the output filenames."
  [fpath flt-keys]
  (let [filters    (!!/listify flt-keys)
        data-in    (weka/load-arff fpath)
        data-mid   (reduce #(filter-instances %1 %2) data-in filters)   ; Apply filter(s)
        data-out   (filter-instances data-mid :attrs)                   ; Remove text attr
        tag        (str/join "." (map #(str (name %)) filters))]
    (log/debug "Filter<" tag ">:" (.numAttributes data-in)  "x" (.size data-in)
               "==>"              (.numAttributes data-out) "x" (.size data-out))
    (weka/save-results fpath tag data-out)))



;;; --------------------------------------------------------------------------
(defn regress
  "Runs a Linear Regression model on the ARFF at the specified filepath."
  [{arff      :arff
   target     :target
   excl-attrs :exclude
   work-csv   :work_csv}]

  (log/info "Excluding attributes:" excl-attrs)
  (try
    (let [insts0 (weka/load-arff arff target)
          insts  (if excl-attrs
                     (filter-instances insts0 (RemoveByName.) ["-E" excl-attrs])
                     insts0)]

      ;; Since Weka leaves out some statistical measures, make a work CSV for Incanter
      (when work-csv
        (weka/save-file work-csv insts :csv))

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
;;; Affective Tweets functionality
;;; TODO: Move to namespace weka.tweets
;;; --------------------------------------------------------------------------
(defn prep-base-emoter
  "Configures the base features for the Affective Tweets filter superclass
  TweetToFeatureVector."
  ([emoter]
  (prep-base-emoter emoter (cfg/? :emote)))


  ([^TweetToFeatureVector emoter
   {:keys [nlp text-index]}]
  ;; Our only defined NLP option is English-Stoplist-Porter
  (when (= nlp :english)
    (let [stoplist (doto (WordsFromFile.)
                         (.setStopwords (io/file STOPLIST-EN)))]
      ;; Prepare the filter for English texts
      (doto emoter
            (.setToLowerCase true)
           ;(.setStandarizeUrlsUsers true)
           ;(.setReduceRepeatedLetters true)
            (.setStopwordsHandler stoplist)
            (.setStemmer (SnowballStemmer. "english")))))

  (doto emoter
        (.setTextIndex (str text-index)))))



;;; --------------------------------------------------------------------------
(defprotocol Emoter
  "Functionality for the Affective Tweets sentiment/emotion filters."
  (prep-emoter [emoter cfg] "Configures the base features of a Affective Tweets filter."))

(extend-protocol Emoter

  ;; -------------------------------------------------------------------------
  TweetToInputLexiconFeatureVector
  (prep-emoter [emoter
                {:keys [nlp] :as cfg}]
  ;; Set up the superclass
  (prep-base-emoter emoter cfg)

  ;; Our only defined NLP option is English-Stoplist-Porter
  (when (= nlp :english)
    (let [lexer (doto (ArffLexiconEvaluator.)
                      (.setStemmer (SnowballStemmer. "english")))]

      ;; Prepare the filter for English texts
      (doto emoter
            (.setLexiconEval (into-array ^ArffLexiconEvaluator [lexer])))))

  ;; Return the configured (mutated) filter
  emoter)


  ;; -------------------------------------------------------------------------
  TweetToGenderFeatures
  (prep-emoter [emoter cfg]
                
  ;; Set up the superclass and index values from the emote configuration
  (prep-base-emoter emoter cfg)
  (doto emoter
        (.setScreenNameIndex  (str (:screen-name-index cfg)))
        (.setFullNameIndex    (str (:full-name-index   cfg)))
        (.setDescriptionIndex (str (:description-index cfg)))
        (.setUsesEMNLP2014    true))))



;;; --------------------------------------------------------------------------
;;; Sila Erlang command functionality
;;; --------------------------------------------------------------------------
(defn emote-arff
  "Reads in an ARFF file with tweets, applies embedding/sentiment/emotion filters,
  as needed for «Say Sila», and then outputs the results in ARFF and CSV formats."
  [fpath]
  (log/debug "Emoting:" fpath)
  (let [emoter    (prep-emoter (TweetToInputLexiconFeatureVector.)
                               (cfg/? :emote))
        data-in   (weka/load-arff fpath)
        data-mid  (filter-instances data-in  emoter (:options (:bws FILTERS)))
        data-out  (filter-instances data-mid :attrs)]

    (log/debug "emote lexicon:" TweetToInputLexiconFeatureVector/NRC_AFFECT_INTENSITY_FILE_NAME)
    (weka/save-results fpath "emote" data-out)))



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

  (let [big-data    (weka/load-arff big-fpath)
        reg-data    (weka/load-arff reg-fpath)

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

    {:arff (weka/save-file "/tmp/dic9315.ML.arff" data-out :arff)
     :csv  (weka/save-file "/tmp/dic9315.ML.csv"  data-out :csv)}))


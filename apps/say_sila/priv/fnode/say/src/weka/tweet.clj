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
  (:require [say.genie       :refer :all]
            [say.config      :as cfg]
            [say.log         :as log]
            [weka.core       :as weka]
            [clojure.java.io :as io]
            [clojure.string  :as str])
  (:import  [affective.core LexiconEvaluator
                            ArffLexiconEvaluator
                            PolarityLexiconEvaluator
                            SWN3LexiconEvaluator]
            [java.util  Random]
            [weka.core  DenseInstance
                        Instance
                        Instances]
            [weka.classifiers AbstractClassifier
                              Evaluation]
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
                                                 TweetToGenderFeatures]
            [weka.filters.unsupervised.instance Resample]))
(set! *warn-on-reflection* true)

(def ^:const RNG-SEED    1)                             ; Weka's default random seed
(def ^:const CV-FOLDS    10)                            ; Folds for cross-validation (<1 means eval dataset)
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

(def ^:const LEXICONS   {:liu   {:typ PolarityLexiconEvaluator :fp TweetToLexiconFeatureVector/BING_LIU_FILE_NAME}
                         :mpqa  {:typ PolarityLexiconEvaluator :fp TweetToLexiconFeatureVector/MPQA_FILE_NAME}
                         :swn   {:typ SWN3LexiconEvaluator     :fp TweetToLexiconFeatureVector/SENTIWORDNET_FILE_NAME}})


;;; --------------------------------------------------------------------------
(defprotocol Lexify

  "Functionality for the Affective Tweets lexicons."
  (eval-token [lex tok]
              [lex tok pos neg]
   "Determines the polartiy score of token (word, emoticon, etc.).
    The caller may specify the desired return values, rather than the
    default :positive, :negative.  The function may also returns nil
    for neutral/not-found)."))


(extend-protocol Lexify

  affective.core.PolarityLexiconEvaluator
  (eval-token
    ([lex tok]
    (eval-token lex tok :positive :negative))

    ([lex tok pos neg]
    (case (.retrieveValue lex tok)
      "positive"  pos
      "negative"  neg
      "not_found" nil)))


  affective.core.SWN3LexiconEvaluator
  (eval-token
    ([lex tok]
    (eval-token lex tok :positive :negative))

    ([lex tok pos neg]
    ;; The basic evaluator behaviour is to evaluate a list of tokens
    (let [{pval "swn-posScore"
           nval "swn-negScore"} (.evaluateTweet lex [tok])
           score                (+ pval nval)]

      ;; TODO: We need to handle dual-polarity
      (when-not (or (zero? pval)
                    (zero? nval))
        (log/fmt-warn "Token '~a' has dual-polarity: p[~a] n[a] score[~a]"
                      tok pval nval score))
      (cond
        (pos? score) pos
        (neg? score) neg)))))



;;; --------------------------------------------------------------------------
(defmacro make-lexicon
  "Returns a sentiment polarity lexicon evaluator as implemented in the
  AffectiveTweets Weka plugin.  Choices are :liu, :mpqa, and :swn"
  [tag]
  (let [ltag        (eval tag)          ; Lexicon tag (often via config lookup)
        lname       (name ltag)
        {ltype :typ
         fpath :fp} (LEXICONS ltag)]
    ;; Common constructor/initialization for the various LexiconEvaluators
    `(do (log/info "Using lexicon:" ~ltype)
         (doto (new ~ltype ~fpath ~lname)
               (.processDict)))))



;;; --------------------------------------------------------------------------
(defn ^SnowballStemmer make-stemmer
  "Returns a Snowball stemmer for the specified language (default: english)."
  ([]
  (make-stemmer "english"))


  ([lang]
  (SnowballStemmer. lang)))


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
  (let [filters    (listify flt-keys)
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
  [{:keys [datasets data_mode eval_mode exclude learner target work_csvs]
    :or   {learner "lreg"}
    :as   conf}]

  (log/info "Excluding attributes:" exclude)
  (try
    ;; TODO: Collapse to ONE letfn over a let
    (letfn [;; ---------------------------------------------------------------
            (calc-variations [fpath ^Instances idata]
              ;; The instances should already be sorted, but now it's crucial!
              (.stableSort idata 0)
              (let [attrs (range 1 (.numAttributes idata))                  ; Skip timestamp
                    odata (doto (Instances. idata (int 0))
                                (.setRelationName (str (.relationName idata) ".variation")))
                    ins    (enumeration-seq (.enumerateInstances idata))]
                (reduce (fn [^Instance prev
                             ^Instance curr]
                          (let [oinst ^Instance (doto (DenseInstance. curr)
                                                      (.setDataset odata))]
                            ;; NOTE: Normally for relative change, we'd want to divide by the
                            ;;       previous value (pv), but we may have attribute values of
                            ;;       zero, for which the relative change is not defined.
                            (doseq [^Integer a attrs]
                              (let [cv (.value curr a)
                                    pv (.value prev a)]
                              (.setValue oinst a (- cv pv))))
                            (.add odata oinst)      ; Variation instance goes to output
                            curr))                  ; The current becomes the "next previous"
                        (first ins)
                        (rest  ins))
                ;; Save a copy for sanity checks
                (weka/save-results fpath :var odata)
                odata))

            ;; ---------------------------------------------------------------
            (load-data [tag]
              (let [fpath   (datasets tag)
                    vmode?  (= data_mode "variation")
                    dset    (as-> (weka/load-arff fpath target) data
                                  (if vmode?  (calc-variations fpath data) data)
                                  (if exclude ^Instances (filter-instances data (RemoveByName.) ["-E" exclude])
                                              ^Instances data))]
                ;; Select the last attribute there's no target
                (when-not target
                  (.setClassIndex dset (dec (.numAttributes dset))))

                (log/fmt-info "Loaded ~a dataset: tgt[~a] cnt[~a] src[~a] mode[~a]",
                              (name tag)
                              (.name (.classAttribute dset))
                              (.numInstances dset)
                              fpath
                              (if vmode? "var" "lvl"))
                dset))


            ;; ---------------------------------------------------------------
            (split-data [insts tag p100]
              ;; Determine the required train/test dataset configuration
              (let [->pct #(str (Math/round (double (* 100 %))))
                    flt   (Resample.)                                   ; Seed (-S) is 1
                    pct   (->pct p100)
                    opts  ["-Z" pct "-no-replacement"]
                    inv?  (= :train tag)
                    split (weka/filter-instances insts flt (if inv?
                                                               (into-array (conj opts "-V"))
                                                               opts))]
                (log/fmt-debug "Sampling ~a @ ~a% for parameter optimization: cnt[~a]"
                               tag
                               (if inv? (->pct (- 1 p100)) pct)
                               (.numInstances split))
                split))


            ;; ---------------------------------------------------------------
            (prep-data [insts]
              ;; Determine the required train/test dataset configuration
              ;; NOTE: a nil test dataset will mean cross-validation
              (let [split   #(let [ds (% datasets)]
                               (when (number? ds) ds))                  ; Split p100|nil
                    sp100   (some split [:parms :test])
                    trains  (if sp100 (split-data insts :train sp100) insts)
                    dload   #(if-let [p100 (split %)]
                               [trains (split-data insts % p100)]
                               [insts  (load-data %)])]

                (case eval_mode
                  "parms" (dload :parms)
                  "test"  (dload :test)
                  "cv"    [insts (log/fmt-info "Using ~a-fold cross validation" CV-FOLDS)]
                          [insts (log/fmt-warn "Invalid evaluation mode: ~a" eval_mode)])))]

      ;; What we load depends
      (let [^Instances insts    (load-data :train)
            [^Instances trains
             ^Instances tests]  (prep-data insts)]

        ;; Since Weka leaves out some statistical measures, make a work CSV for Incanter
        ;; TODO: Incanter will need the testers as well
        (when work_csvs
          (weka/save-file (:train work_csvs) trains :csv))

        ;; Use N-fold cross validation for training/evaluation
        (let [model       (weka/make-learner learner)
              audit       (Evaluation. trains)
              attr-coeff  (fn [[ndx coeff]]
                            (let [attr (.attribute ^Instances insts (int ndx))
                                  tag  (.name attr)]
                              [tag coeff]))]

          ;; The final model training always uses the full dataset
          (.buildClassifier model trains)
          (log/fmt-info "Model [~a]:\n~a" (type model) (str model))

          ;; How do they want the results evaluated?
          (if tests
              (.evaluateModel audit model ^Instances tests NO-OBJS)
              (.crossValidateModel audit model trains CV-FOLDS (Random. RNG-SEED)))
          (log/info "Summary:\n" (.toSummaryString audit))

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
                coeffs      (if (= (type model) LinearRegression)
                                (.coefficients ^LinearRegression model)
                                [])
                [coeff-cnt
                 intercept] (if (empty? coeffs)
                                [0 0]                                       ; Unexplainable!
                                [(- (count coeffs) 2), (last coeffs)])]     ; See note above
            (assoc ACK :model        (type model)
                       :instances    (.numInstances trains)
                       :correlation  (.correlationCoefficient audit)
                       :error_mae    (.meanAbsoluteError audit)
                       :error_rmse   (.rootMeanSquaredError audit)
                       :intercept    intercept
                       :coefficients (into {} (map attr-coeff                       ; ZIP:
                                                   (map vector (range coeff-cnt)    ; Attr-indexes
                                                               coeffs))))))))       ; Weights
    (catch Exception ex
           (log/fail ex "Regression failed" :stack)
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
           ;(.setReduceRepeatedLetters true)
            (.setStopwordsHandler stoplist)
            (.setStemmer (SnowballStemmer. "english")))))

  ;; Set our standard options for both the NLP and default modes
  (doto emoter
        (.setToLowerCase true)
       ;(.setReduceRepeatedLetters true)
       ;(.setStandarizeUrlsUsers true)
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


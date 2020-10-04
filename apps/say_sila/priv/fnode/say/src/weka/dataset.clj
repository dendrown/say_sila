;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Utilities to convert between various say-sila ARFF formats.
;;;;
;;;; While the Say-Sila is in the intial research phase, we can expect ARFF
;;;; formats to evolve and inconsistencies to occur.  This module allows for
;;;; quick-n-dirty bridging between existing functionality and new exploration
;;;; until data formats are ready for (relatively stable) formalization.
;;;;
;;;; Dataset codes:
;;;;    - S : say-senti (sentiment/emotion analysis)
;;;;    - T : tweet extraction (from Erlang)
;;;;    - U : user analysis
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.dataset
  (:require [say.genie       :refer :all]
            [say.config      :as cfg]
            [say.log         :as log]
            [weka.core       :as weka]
            [weka.tweet      :as tw]
            [clojure.string  :as str]
            [clojure.pprint  :refer [pp]])
  (:import  (weka.core  Attribute
                        Instance
                        Instances)
            (weka.filters.unsupervised.instance RemoveDuplicates
                                                SubsetByExpression)))

(set! *warn-on-reflection* true)

(def ^:const Codes  {:senti :s
                     :tweet :t
                     :user  :u})


;;; --------------------------------------------------------------------------
(defn- col-map
  "Create 0-based column map for an ARFF format. For example:
    {:id 0, :lang 1, :screen_name 2, :name 3, :description 4, :text 5}"
  [attrs]
  (into {} (map vector attrs (range (count attrs)))))

;;; Current dataset layouts; where for the X99 codes:
;;; - X is the dataset content code, and
;;; - the highest 99 value represents the latest version
(defonce Datasets   {:s :s01    ; TODO :s02     ; [S]tatus text [s]entiment/emotion
                     :t :t00                    ; [T]witter input (from Sila/erl)
                     :u :u00})                  ; [U]ser information


;;; Column names generally correspond to Twitter's status (meta)data keys
(defonce S02-cols   (col-map [:id :screen_name :text :green]))
(defonce S01-cols   (col-map [:id :screen_name :text :sentiment]))
(defonce S00-cols   (col-map [:id :text :sentiment]))

(defonce T00-cols   (col-map [:id :lang :screen_name :name :description :text]))
(defonce U00-cols   (col-map [:screen_name :name :description :environmentalist]))

;;; Column/attribute lookup by dataset
(defonce Columns    {:s02 S02-cols
                     :s01 S01-cols
                     :s00 S00-cols
                     :t00 T00-cols
                     :u00 U00-cols})


;;; --------------------------------------------------------------------------
(defn code
  "Returns the current dataset format code in string form for the specified
  short or long data tag."
  [dtag]
  (let [dset (or (Datasets dtag)
                 (Datasets (Codes dtag)))]
    (when dset
      (KEYSTR dset))))



;;; --------------------------------------------------------------------------
(defn columns
  "Returns the column index for the specified dataset format and column tag."
  [dtag]
  (Columns (or (Datasets dtag) dtag)))



;;; --------------------------------------------------------------------------
(defn col-index
  "Returns the column index for the specified dataset format and column tag."
  [dset col & opts]
  (let [col (get-in Columns [dset col])]
     ;; Default is to return a 0-based column
     (if (some #{:1-based} opts)
         (weka/index0->1 col)
         col)))


;;; --------------------------------------------------------------------------
(defn col-target
  "Returns the key for the last (presumably the target) column for the
  specified data format code."
  [dset]
  (last (keys (Columns dset))))



;;; --------------------------------------------------------------------------
(defn col-diff
  "Returns a list of column keys dataset a that are not part of dataset b."
  [a b]
  (let [[akeys
         bkeys] (map #(keys (Columns %)) [a b])]
    ;; The order will probably need to be reversed, but we don't do it here.
    (remove (into #{} bkeys) akeys)))



;;; --------------------------------------------------------------------------
(defn- append-col
  "Adds a new column as the last attribute of the specified instances."
  [^Instances insts tag]
  (.insertAttributeAt insts
                      (Attribute. (name tag))
                      (.numAttributes insts)))



;;; --------------------------------------------------------------------------
(defn- delete-col
  "Removes an attribute column, specified by a dataset key (dset) and
  a column key (col)."
  [^Instances insts dset col]
  (.deleteAttributeAt insts (col-index dset col)))



;;; --------------------------------------------------------------------------
(defn ^Instances prep-dataset
  "Loads the instances in the specified ARFF which must correspond to the
  specified dataset structure.  The function returns the instances after
  deleting the columns in dels and adding the columns in adds."
  [data in out & targets]
  ;; Remove extra columns in reverse order & add the target w/ unknown values
  (let [insts (weka/load-dataset data)
        dels  (reverse (col-diff in out))       ; Remove attrs in reverse order
        adds  (if targets                       ; FIXME: Target should be nominal
                  targets
                  [(col-target out)])]
    (run! #(delete-col insts in %) dels)
    (run! #(append-col insts %) adds)
    insts))



;;; --------------------------------------------------------------------------
(defn- ^Instances process-text
  "Accepts a set of instances and returns a corresponding set where the values
  for the requested attribute have been run through lexicon and part of speech
  filters."
  ([insts dset]
  (process-text insts dset :text))


  ([insts dset col & opts]
  (let [lex-tag (cfg/?? :senti :lexicon :liu)
        txt-ndx (col-index dset col :1-based)
        dataset (if (some #{:ensure-text} opts)
                    (weka/filter-instances insts
                                           (SubsetByExpression.)
                                           ["-E" (strfmt "not(ATT~a is '')" txt-ndx)])
                    insts)]

    (reduce #(weka/filter-instances %1 %2)
            dataset
            [(tw/make-lexicon-filter lex-tag txt-ndx)       ; Senti/emo
             (tw/make-tagging-filter txt-ndx)]))))          ; POS tags



;;; --------------------------------------------------------------------------
(defn- ^Instances t00->s02
  "Converts the T00 tweet format to the S00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S02»."
  [insts]
  (-> (prep-dataset insts :t00 :s02)
      (process-text :s02)))



;;; --------------------------------------------------------------------------
(defn- ^Instances t00->s01
  "Converts the T00 tweet format to the S00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S01»."
  [insts]
  (-> (prep-dataset insts :t00 :s01)
      (process-text :s01)))



;;; --------------------------------------------------------------------------
(defn- ^Instances t00->s00
  "Converts the T00 tweet format to the S00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S00»."
  [insts]
  (-> (prep-dataset insts :t00 :s00)
      (process-text :s00)))                                             ; Emo/POS on tweet text



;;; --------------------------------------------------------------------------
(defn- ^Instances t00->u00
  "Converts the T00 tweet format to the U00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «U00»."
  [insts]
  (-> (prep-dataset insts :t00 :u00)
      (weka/filter-instances (RemoveDuplicates.))       ; One per user/profile
      (process-text :u00 :description :ensure-text)))   ; Emo/POS on user profile



;;; --------------------------------------------------------------------------
(defn transform
  "Converts the current T99 tweet format to the current specified target format.
  The function creates a copy of the specified dataset whose filename is tagged
  to indicate the output data structure."
  ([arff dset xform]
  (let [insts (weka/load-arff arff)]
    (transform arff insts dset xform)))


  ([arff insts dset xform]
  ;; The ARFF and instance data should match!
  (weka/save-file arff
                  (KEYSTR dset)                         ; Tag new ARFF
                  (xform insts)                         ; Convert dataset
                  :arff)))



;;; --------------------------------------------------------------------------
(defmacro defn-transform
  "Creates a transformation function with a name like « t->d » from the
  tweet (T99) dataset to the current version of the dataset d specified
  by dset."
  [dset]
  (let [dtag  (eval dset)
        [t99
         d99] (map #(Datasets %) [:t dtag])
        xform (symbol (apply str (map name [t99 "->" d99])))
        t->d  (symbol (str "t->" (name dtag)))]
    ;; Create a wrapper function that calls the existing transformation function
    `(defn ~t->d
       ;; Auto-load data for a single thread of execution
       ([arff#]
       (transform arff# ~d99 ~xform))

       ;; Preloaded instances for concurrent execution
       ([arff# insts#]
       (transform arff# insts# ~d99 ~xform)))))

(defn-transform :s)     ; fn: t->s
(defn-transform :u)     ; fn: t->u



;;; --------------------------------------------------------------------------
(defn t->su
  "Concurrently transforms a tweet ARFF (T99) to the most recent senti (S99)
  and user (U99) formats."
  [arff]
  ;; Load the ARFF once and create copies for the transformations
  ;; FIXME: Results with pmap are showing weird look-alike differences.
  (let [insts (weka/load-arff arff)]
    (map #(% arff (Instances. insts)) [t->s             ; TODO: pmap
                                       t->u])))


;;; --------------------------------------------------------------------------
(defn count-user-tweets
  "Lists user tweet counts in descending order for the specified dataset.
  The caller may give a minimum tweet count for a user to be included in
  the report."
  ([dset data]
  (count-user-tweets dset data 1))


  ([dset data mincnt]
  (let [insts (weka/load-dataset data)
        col   (col-index (Datasets :t) :screen_name)
        users (reduce #(update %1 (.stringValue ^Instance %2 (int col)) ; Key: screen name
                                  (fnil inc 0))                         ; Val: tweet count
                      {}
                      (weka/instance-seq insts))]

    ;; Print out a simple report listing, the user screen names and counts
    (doseq [[usr cnt] (take-while #(>= (second %) mincnt)                       ; Use requested count
                                  (sort-by second #(compare %2 %1) users))]     ; Count order:  Z..A
      (log/fmt-info "~24a: ~a" usr cnt)))))


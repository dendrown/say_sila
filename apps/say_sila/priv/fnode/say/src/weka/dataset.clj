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
            [clojure.pprint  :refer [pp]]
            [defun.core      :refer [defun defun-]])
  (:import  (weka.core  Attribute
                        Instances)
            (weka.filters.unsupervised.instance RemoveDuplicates
                                                SubsetByExpression)))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
(defn- col-map
  "Create 0-based column map for an ARFF format. For example:
    {:id 0, :lang 1, :screen_name 2, :name 3, :description 4, :text 5}"
  [attrs]
  (into {} (map vector attrs (range (count attrs)))))

;;; Column names generally correspond to Twitter's status (meta)data keys
(defonce S00-cols   (col-map [:id :text :sentiment]))
(defonce T00-cols   (col-map [:id :lang :screen_name :name :description :text]))
(defonce U00-cols   (col-map [:screen_name :name :description :environmentalist]))

;;; Column/attribute lookup by dataset
(defonce Columns    {:s00 S00-cols
                     :t00 T00-cols
                     :u00 U00-cols})

;;; Current dataset layouts
(defonce Datasets   {:s :s00
                     :t :t00
                     :u :u00})


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
(defun ^Instances prep-dataset
  "Loads the instances in the specified ARFF which must correspond to the
  specified dataset structure.  The function returns the instances after
  deleting the columns in dels and adding the columns in adds."
  ([arff :guard string? dset dels adds]
  (prep-dataset (weka/load-arff arff) dset dels adds))


  ([insts dset dels adds]
  ;; Remove extra columns in reverse order & add the target w/ unknown values
  (run! #(delete-col insts dset %) dels)
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
(defn- ^Instances t00->s00
  "Converts the T00 tweet format to the S00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S00»."
  [insts]
  (-> (prep-dataset insts :t00 [:description :name :screen_name :lang]  ; dels: reverse order
                               [:sentiment])                            ; adds: normal order
      (process-text :s00)))                                             ; Emo/POS on tweet text



;;; --------------------------------------------------------------------------
(defn- ^Instances t00->u00
  "Converts the T00 tweet format to the U00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «U00»."
  [insts]
  (-> (prep-dataset insts :t00 [:text :lang :id]        ; dels: reverse order
                               [:environmentalist])     ; adds: normal order
      (weka/filter-instances (RemoveDuplicates.))       ; One per user/profile
      (process-text :u00 :description :ensure-text)))   ; Emo/POS on user profile



;;; --------------------------------------------------------------------------
(defun transform
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


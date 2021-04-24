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
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [weka.core          :as weka]
            [weka.tweet         :as tw]
            [clojure.data.json  :as json]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp]])
  (:import  (weka.core  Attribute
                        DenseInstance
                        Instance
                        Instances)
            (weka.filters.unsupervised.instance RemoveDuplicates
                                                SubsetByExpression)))

(set! *warn-on-reflection* true)

(def ^:const Init-Stances   "/tmp/say_sila/stances.json")
(def ^:const Codes          {:senti :s
                             :tweet :t
                             :user  :u})


;;; --------------------------------------------------------------------------
(defn- warn-if-not-dataset
  "Logs a warning if the given dataset is not the needed dataset"
  [needed given]
  (when (not= needed given)
    (log/fmt-warn "Dataset ~a is not currently supported (only ~a)"
                  (KEYSTR given) (KEYSTR needed))))


;;; --------------------------------------------------------------------------
(defn- col-map
  "Create 0-based column map for an ARFF format. For example:
    {:id 0, :lang 1, :screen_name 2, :name 3, :description 4, :text 5}"
  [attrs]
  (into {} (map vector attrs (range (count attrs)))))

;;; Current dataset layouts; where for the X99 codes:
;;; - X is the dataset content code, and
;;; - the highest 99 value represents the latest version
(defonce Datasets   {:g :g01            ; [G]reen/denier machine learning target
                     :s :s03            ; [S]tatus text [s]entiment/emotion
                     :t :t02            ; [T]witter input (from Sila/erl)
                     :u :u01})          ; [U]ser information


;;; Column names generally correspond to Twitter's status (meta)data keys
(defonce G00-Cols   (col-map [:screen_name
                              ; TODO: affect
                              ; TODO: weak indicator accounts
                              :stance]))
(defonce G01-Cols   (col-map [:screen_name
                              ; TODO: affect
                              ; TODO: indicatork+strong  accounts
                              :stance]))

(defonce S00-Cols   (col-map [:id :text :sentiment]))
(defonce S01-Cols   (col-map [:id :screen_name :text :sentiment]))
(defonce S02-Cols   (col-map [:id :screen_name :text :stance]))
(defonce S03-Cols   (col-map [:id :date :screen_name :text :stance]))

(defonce T00-Cols   (col-map [:id :lang :screen_name :name :description :text]))        ; NOTE: lang may be date
(defonce T01-Cols   (col-map [:id :lang :screen_name :name :description :text :stance]))
(defonce T02-Cols   (col-map [:id :date :screen_name :name :description :text :stance]))

(defonce U00-Cols   (col-map [:screen_name :name :description :environmentalist]))
(defonce U01-Cols   (col-map [:screen_name :name :description :stance]))

;;; Column/attribute lookup by dataset
(defonce Columns    {:g00 G00-Cols
                     :g01 G01-Cols
                     ;--------------
                     :s00 S00-Cols
                     :s01 S01-Cols
                     :s02 S02-Cols
                     :s03 S03-Cols
                     ;--------------
                     :t00 T00-Cols
                     :t01 T01-Cols
                     :t02 T02-Cols
                     ;--------------
                     :u00 U00-Cols
                     :u01 U01-Cols})


;;; --------------------------------------------------------------------------
(defn which-code
  "Returns the current dataset key code, given the short code format."
  [dtag]
  (or (Datasets dtag)                       ; Short identifier    - :s
      (Datasets (Codes dtag))               ; Human identifier    - :senti
      (some #{dtag} (keys Columns))))       ; Explicit identifier - :s01



;;; --------------------------------------------------------------------------
(defn code
  "Returns the current dataset format code in string form for the specified
  short or long data tag."
  [dtag]
  (when-let [dset (which-code dtag)]
    (KEYSTR dset)))



;;; --------------------------------------------------------------------------
(defn columns
  "Returns the column index for the specified dataset format and column tag."
  [dtag]
  (Columns (or (Datasets dtag) dtag)))



;;; --------------------------------------------------------------------------
(defn ^Long col-index
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
  (last (keys (Columns (which-code dset)))))



;;; --------------------------------------------------------------------------
(defn col-diff
  "Returns a list of column keys dataset a that are not part of dataset b."
  [a b]
  (let [[akeys
         bkeys] (map #(keys (Columns %)) [a b])]
    ;; The order will probably need to be reversed, but we don't do it here.
    ;(log/debug a ":" akeys)
    ;(log/debug b ":" bkeys)
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
        adds  (if targets targets [])]          ; FIXME: Target should be nominal
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
  (let [lex-tag (cfg/?? :sila :lexicon :liu)
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
(defn- ^:deprecated ^Instances t00->s00
  "Converts the T00 tweet format to the S00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S02»."
  [insts]
  (-> (prep-dataset insts :t00 :s00 :sentiment)
      (process-text :s00)))                                             ; Emo/POS on tweet text



;;; --------------------------------------------------------------------------
(defn- ^:deprecated ^Instances t00->s01
  "Converts the T00 tweet format to the S01 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S01»."
  [insts]
  (-> (prep-dataset insts :t00 :s01 :sentiment)
      (process-text :s01)))



;;; --------------------------------------------------------------------------
(defn- ^Instances t01->s02
  "Converts the T01 tweet format to the S02 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S02»."
  [insts]
  (-> (prep-dataset insts :t01 :s02)
      (process-text :s02)))



;;; --------------------------------------------------------------------------
(defn- ^Instances t02->s03
  "Converts the T02 tweet format to the S03 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S03»."
  [insts]
  (-> (prep-dataset insts :t02 :s03)
      (process-text :s03)))



;;; --------------------------------------------------------------------------
(defn- ^:deprecated ^Instances t00->u00
  "Converts the T00 tweet format to the U00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «U00»."
  [insts]
  (-> (prep-dataset insts :t00 :u00 :environmentalist)
      (weka/filter-instances (RemoveDuplicates.))       ; One per user/profile
      (process-text :u00 :description :ensure-text)))   ; Emo/POS on user profile



;;; --------------------------------------------------------------------------
(defn- ^Instances t01->u01
  "Converts the T01 tweet format to the U01 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «U00»."
  [insts]
  (-> (prep-dataset insts :t01 :u01)
      (weka/filter-instances (RemoveDuplicates.))       ; One per user/profile
      (process-text :u01 :description :ensure-text)))   ; Emo/POS on user profile



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
;(defn-transform :u)    ; fn: t->u ; TODO: reinstate U99



;;; --------------------------------------------------------------------------
(defn t->su
  "Concurrently transforms a tweet ARFF (T99) to the most recent senti (S99)
  and user (U99) formats."
  [arff]
  ;; Load the ARFF once and create copies for the transformations
  ;; FIXME: Results with pmap are showing weird look-alike differences.
  (let [insts (weka/load-arff arff)]
    (log/info "Not currently creating the" (KEYSTR (Datasets :u)) "dataset")
    (map #(% arff (Instances. insts)) [t->s         ; TODO: pmap
                                      ;t->u         ; TODO: reinstate U99
                                       ])))



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



;;; --------------------------------------------------------------------------
(defn load-stances
  "Loads a mapping of stances (`green' or `denier') from the specified
  file path."
  ([]
  (load-stances nil))


  ([fpath]
  (let [fpath (if fpath
                  fpath
                  Init-Stances)]
    (update-values (json/read-str (slurp fpath)) set))))



;;; --------------------------------------------------------------------------
(defn target-stance!
  "Inserts the stance {green,denier} column to the dataset (updated in place)
  at the specified index (defaults to appending the stance column to the end)."
  ([^Instances insts]
  (target-stance! insts (.numAttributes insts)))        ; Append as last column


  ([^Instances insts ndx]
  (doto insts
        (.insertAttributeAt (Attribute. (name (col-target :t01))
                                        ["green" "denier"])
                             ndx)
        (.setClassIndex ndx))))



;;; --------------------------------------------------------------------------
(defn label!
  "Add dependent class information to an unlabelled dataset.  Currently we
  are supporting this procedure only as a T00==>T01 transformation.  If data
  represents a filepath, the function loads the associated ARFF, creates a
  corresponding 'T01' tagged output ARFF, and returns the filepath to it.
  If data is a set of Instances, the function makes a copy of this dataset,
  performs the transformation on this copy, and returns it.  Note that rows
  with screen_name values not found in the stances map will not be included
  in the final dataset."
  ([dset data]
  (label! dset data nil))


  ([dset data stances]
  (warn-if-not-dataset :t00 dset)
  (let [arff?   (string? data)
        stances (if (map? stances)
                    stances                             ; KV: stance => screen_name
                    (load-stances stances))             ; Pull map from file
        insts   ^Instances
                (if arff?
                    (weka/load-arff data)               ; Load dataset from file
                    (Instances. ^Instances data))       ; Copy input instances
        nndx    (col-index dset :screen_name)           ; Screen [n]ame index
        who     #(.stringValue ^Instance % (int nndx))  ; Screen name lookup
        stand   #(reduce (fn [_ [stance accts]]         ; Find stance of account %
                           (when (contains? accts %)
                             (reduced stance)))
                         nil
                         stances)]
    ;; We're altering the output dataset in place!
    (target-stance! insts)

    ;; Set a green|denier stance for known users
    (dotimes [i (.numInstances insts)]
      (let [row (.instance insts i)]
        (when-let [stance ^String (stand (who row))]
          (.setClassValue row stance))))

    ;; Remove users for whom we don't know where they stand
    (.deleteWithMissingClass insts)

    ;; Prepare results as an ARFF or dataset per the original parameters
    (if arff?
      ;; Save the dataset and just return the path
      (let [fpath (weka/tag-filename data (KEYSTR :t01))]
        (weka/save-file fpath insts)
        fpath)
      ;; Data IN, so data OUT
      insts))))



;;; --------------------------------------------------------------------------
(defn combine-days
  ""
  ([dset data]
  (combine-days dset data nil))

  ([dset data n]
  (warn-if-not-dataset :s03 dset)
  (let [arff?   (string? data)
        target  (col-target dset)
        sname   (col-index dset :screen_name)
        iinsts  (if arff?
                    (weka/load-arff data (name target))     ; Load for our use
                    (Instances. ^Instances data ))          ; Copy of original
        oinsts  (Instances. iinsts 0)                       ; Output w/ headers
        start   (inc (col-index dset target))               ; Start of emo attrs
        stop+1  (.numAttributes iinsts)                     ; No more emo attrs
        date    (col-index dset :date)

        ;; Returns an instance with the combined affect attributes of the input instances
        combine (fn [^Instance i1
                     ^Instance i2]
                  ;; Sum all the affect attributes
                  (reduce (fn [^Instance inst
                               ^Long ndx]
                            (.setValue inst ndx (+ (.value i1 ndx)
                                                   (.value i2 ndx)))
                            inst)
                          (DenseInstance. i1)
                          (range start stop+1)))

        ;; Create a map, keyed by the date, of instances combined by day
        [users
         combs] (reduce (fn [[users combs] ii]
                          (let [inst   (.get iinsts (int ii))
                                day    (.stringValue inst date)    ; Key to instance map
                                users* (conj users (.stringValue inst sname))
                                combs* (if (contains? combs day)
                                           (update combs day combine inst)
                                           (assoc combs day inst))]
                            [users* combs*]))
                        [#{} (sorted-map)]
                        (range (.numInstances iinsts)))

        ;; Normalize the affect levels with respect to the user count
        num-users (count users)
        non-users (filter empty? users)
        norm      (if n n num-users)
        normalize (fn [^Instance inst]
                    (doseq [^Long a (range start stop+1)]
                      (.setValue inst a (/ (.value inst a)
                                           norm))))]

    (log/info "Combining affect across days for" num-users "users")
    (log/info "Normalization factor:" norm)
    (when (not-empty non-users)
      (log/fmt-warn "Dataset contains ~a non-users: ~a" (count non-users) non-users))

    ;; Fill in the output dataset with the instances from our map
    (doseq [[_ inst] combs]
      (normalize inst)
      (.add oinsts inst))

    ;; Tweet id/text and user names make no sense when combining users
    ;;
    ;; NOTE: it *might* be more efficient to remove these columns before combining
    ;;       the instances, though this would add a bit of complexity unless we
    ;;       formalize the output dataset format up at the top.
    (run! #(delete-col oinsts :s03 %) [:text :screen_name :id])     ; Remove in reverse order

    ;; Return filenames|instances according to what we got as parameters
    (if arff?
        (weka/save-results data "DAYS" oinsts)
        oinsts))))


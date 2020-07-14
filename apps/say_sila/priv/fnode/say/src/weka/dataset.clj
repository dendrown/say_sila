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
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns weka.dataset
  (:require [say.genie       :refer :all]
            [say.log         :as log]
            [weka.core       :as weka]
            [clojure.string  :as str])
  (:import  [weka.core  Attribute
                        Instances]))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
(defn- col-map
  "Create 0-based column map for an ARFF format."
  [attrs]
  (into {} (map vector attrs (range (count attrs)))))

;;; Column names generally correspond to Twitter's status (meta)data keys
(defonce S00-cols   (col-map [:id :text :sentiment]))
(defonce T00-cols   (col-map [:id :lang :screen_name :name :description :text]))

(defonce Columns    {:s00 S00-cols
                     :t00 T00-cols})


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
  (.deleteAttributeAt insts (get-in Columns [dset col])))



;;; --------------------------------------------------------------------------
(defn ^Instances t00->s00
  "Converts the T00 tweet format to the S00 say-senti format.  The function
  creates a copy of the specified dataset whose filename is tagged with «S00»."
  [arff]
  (let [insts (weka/load-arff arff)]
    ;; Remove extra columns in reverse order & add the target w/ unknown values
    (run! #(delete-col insts :t00 %) [:description :name :screen_name :lang])
    (append-col insts :sentiment)

    (weka/save-file arff "S00" insts :arff)))


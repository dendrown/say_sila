;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Basic Formal Ontology (BFO) plus support for ontologies in the Open
;;;; Biomedical Ontologies (OBO) Consortium.
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.bfo
  (:require [say.genie      :refer :all]
            [say.log        :as log]
            [say.ontology   :as ont]
            [tawny.repl     :as repl]               ; <= DEBUG
            [tawny.owl      :as owl]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://purl.obolibrary.org/obo/")      ; OBO Consortium
(def ^:const ONT-FPATH  "resources/KB/bfo_v2.0.owl")
(def ^:const ONTOLOGY   (ont/load-ontology ONT-IRI ONT-FPATH))

(def ^:const ZERO-PAD   7)                                      ; Default is BFO padding
(def ^:const PADDING    {:bfo   ZERO-PAD                        ; Zero-padding overrides
                         :mf    6})

(owl/defontology bfo
  :iri    ONT-IRI
  :prefix "bfo")


;;; --------------------------------------------------------------------------
(defmacro redefclass
  "
  Wrapper for say.ontology/redefclass to define human-readable variables that
  point to the BFO_000WXYZ IRI suffixes.
  "
  ([var n]
  `(redefclass :bfo ~var ~n))


  ([ont var n]
  `(let [tag#   (KEYSTR ~ont)
         pad#   (get PADDING ~ont ZERO-PAD)
         fmt#   (str "~a_~" pad# ",'0d")
         obo-n# (log/fmt fmt# tag# ~n)]
     (ont/redefclass ~var ONT-IRI obo-n#))))


;;; --------------------------------------------------------------------------
; Create access variables only for the classes we need
(redefclass entity                  1)
(redefclass continuant              2)
(redefclass independent-continuant  4)
(redefclass material-entity         40)


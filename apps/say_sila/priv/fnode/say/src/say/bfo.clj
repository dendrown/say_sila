;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Basic Formal Ontology (BFO)
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.bfo
  (:require [say.log        :as log]
            [say.ontology   :as ont]
            [tawny.repl     :as repl]               ; <= DEBUG
            [tawny.owl      :as owl]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://purl.obolibrary.org/obo/")
(def ^:const ONT-FPATH  "resources/KB/bfo_v2.0.owl")
(def ^:const ONTOLOGY   (ont/load-ontology ONT-IRI ONT-FPATH))

(owl/defontology bfo
  :iri    ONT-IRI
  :prefix "bfo")


(defmacro redefclass-bfo
  "
  Wrapper for say.ontology/redefclass to define human-readable variables that
  point to the BFO_000WXYZ IRI suffixes.
  "
  [var n]
  `(let [bfo-n# (log/fmt "BFO_~7,'0d" ~n)]
     (ont/redefclass ~var ONT-IRI bfo-n#)))


; Create access variables only for the classes we need
(redefclass-bfo entity            1)
(redefclass-bfo continuant        2)
(redefclass-bfo material-entity  40)


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Friend-of-a-friend (foaf) ontology
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.foaf
  (:require [say.ontology     :as ont]
            [clojure.java.io  :as io]
            [tawny.read       :as rd]
            [tawny.owl :refer :all])
  (:import  [org.semanticweb.owlapi.model IRI]))


(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://xmlns.com/foaf/0.1/")
(def ^:const ONT-FPATH  "resources/KB/foaf.owl")

(def ONTOLOGY (ont/load-ontology ONT-IRI ONT-FPATH))

(defontology foaf
  :iri    ONT-IRI
  :prefix "foaf")

(def Document (ont/get-class ONT-IRI "Document"))

;;; --------------------------------------------------------------------------
(defn load-fully
  "
  Loads the foaf ontology and (theoretically) makes it available via
  normal tawny.owl functionality.
  "
  []
  (rd/defread foaf
    :prefix   "foaf"
    :iri      ONT-IRI
    :location (IRI/create (io/as-file ONT-FPATH))
    :filter   (partial rd/iri-starts-with-filter ONT-IRI)
    :transform
      (comp rd/stop-characters-transform
            rd/exception-nil-label-transform)))

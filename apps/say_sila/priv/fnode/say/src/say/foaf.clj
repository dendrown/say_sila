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
;;;; @copyright 2018-2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.foaf
  (:require [say.ontology    :as ont]
            [clojure.java.io :as io]
            [tawny.owl       :as owl]
            [tawny.repl      :as repl]              ; <= DEBUG
            [tawny.read      :as rd])
  (:import  [org.semanticweb.owlapi.model IRI]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://xmlns.com/foaf/0.1/")
(def ^:const ONT-FPATH  "resources/KB/foaf.owl")
(def ^:const ONTOLOGY   (ont/load-ontology ONT-IRI ONT-FPATH))

(owl/defontology foaf
  :iri    ONT-IRI
  :prefix "foaf")

; Create access variables only for the foaf classes we need
(ont/redefclass Document ONT-IRI)
(ont/redefclass Group    ONT-IRI)



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


;;; --------------------------------------------------------------------------
(load-fully)

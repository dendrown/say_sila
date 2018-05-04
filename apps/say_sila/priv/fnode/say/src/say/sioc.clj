;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Semantically-interlinked online communities (sioc) ontology
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.sioc
  (:require [say.ontology :as ont]
            [tawny.owl    :as owl]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://rdfs.org/sioc/ns#")
(def ^:const ONT-FPATH  "resources/KB/sioc.owl")
(def ^:const ONTOLOGY   (ont/load-ontology ONT-IRI ONT-FPATH))

(owl/defontology sioc
  :iri    ONT-IRI
  :prefix "sioc")

; Create access variables only for the sioc classes we need
(ont/redefclass Post ONT-IRI)
(ont/redefclass Role ONT-IRI)


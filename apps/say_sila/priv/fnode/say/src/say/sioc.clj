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
            [tawny.owl    :refer [defontology]]))


(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://rdfs.org/sioc/ns#")
(def ^:const ONT-FPATH  "resources/KB/sioc.owl")

(def ONTOLOGY (ont/load-ontology ONT-IRI ONT-FPATH))

(defontology sioc
  :iri    ONT-IRI
  :prefix "sioc")

(def Role (ont/get-class ONT-IRI "Role"))


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Say-Sila ontology
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.sila
  (:require [tawny.owl :refer :all]
            [tawny.english   :as dl]
            [tawny.reasoner  :as rsn]
            [clojure.java.io :as io])
    (:import  [org.semanticweb.owlapi.model IRI
                                          OWLOntologyID]))


(set! *warn-on-reflection* true)

; Protégé-crafted ontology
(def ^:const SAY-SILA-IRI   "http://www.dendrown.net/uqam/say-sila")
(def ^:const SAY-SILA-FPATH "resources/KB/say-sila.owl")


;;; --------------------------------------------------------------------------
(defontology say-sila
  :iri    SAY-SILA-IRI
  :prefix "sila")

; TODO:
;   Tweet           ⊑ sioc:Post ⊑ foaf:Document
;   TwitterAccount  ⊑ sioc:UserAccount
;   Player          ⊑ sioc:Role
(defclass Player)


;;; --------------------------------------------------------------------------
(defn load-ss
  "
  Loads the official say-sila ontology
  "
  []
  (let [iri (OWLOntologyID. (IRI/create SAY-SILA-IRI))
        rsc (io/as-file SAY-SILA-FPATH)
        man (owl-ontology-manager)]
    (remove-ontology-maybe iri)
    (.loadOntologyFromOntologyDocument man (IRI/create rsc))))


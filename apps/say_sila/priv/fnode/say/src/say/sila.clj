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
  (:require [say.foaf         :as foaf]
            [say.sioc         :as sioc]
            [clojure.java.io  :as io]
            [tawny.english    :as dl]
            [tawny.reasoner   :as rsn]
            [tawny.owl :refer :all])
    (:import  [org.semanticweb.owlapi.model IRI
                                          OWLOntologyID]))


(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-sila")
(def ^:const ONT-FPATH  "resources/KB/say-sila.owl")


;;; --------------------------------------------------------------------------
(defontology say-sila
  :iri    ONT-IRI
  :prefix "sila")

(defclass Player
  :super    sioc/Role
  :label    "Player"
  :comment  "Active participant during a Say-Sila tracking run")

; TODO:
;   Tweet           ⊑ sioc:Post ⊑ foaf:Document
;   TwitterAccount  ⊑ sioc:UserAccount

(as-disjoint

  (defclass Tweet
    :label   "Tweet"
    :comment "A Twitter message post")

  (defclass TwitterAccount
    :label   "Twitter Account"
    :comment "A user account on Twitter"))

(as-subclasses
  Player :cover
         :disjoint
  (defclass BigPlayer
    :label   "Big Player"
    :comment "A Player (participant) who is extremely active during a tracking run")
  (defclass RegularPlayer
    :label   "Regular Player"
    :comment "A Player (participant) who demonstrates normal activity during a tracking run"))


;;; --------------------------------------------------------------------------
(defn save
  "
  Saves the say-sila ontology in OWL format.
  "
  ([] (save ONT-FPATH))

  ([^String fpath]
  (save-ontology say-sila fpath :owl)))


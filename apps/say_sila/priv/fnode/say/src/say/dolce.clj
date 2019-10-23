;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; DOLCE+D&S Ultralite (DUL) Top-Level Ontology
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.dolce
  (:require [say.genie      :refer :all]
            [say.ontology   :refer :all]
            [say.log        :as log]
            [tawny.repl     :as repl]               ; <= DEBUG
            [tawny.owl      :as owl]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#")
(def ^:const ONT-FPATH  "resources/KB/DUL.owl")
(def ^:const ONTOLOGY   (load-ontology ONT-IRI ONT-FPATH))


;;; --------------------------------------------------------------------------
(owl/defontology dul
  :iri    ONT-IRI
  :prefix "dul")


;;; --------------------------------------------------------------------------
;;; Create access variables only for the classes we need.
;;;
;;; TBox: building on owl:Thing
(redefclass Entity)
(redefclass Abstract)
(redefclass Quality)


;;; TBox: building on dul:Object ⊑ dul:Entity
(redefclass InformationEntity)
(redefclass InformationObject)

(redefclass Agent)
(redefclass Person)
(redefclass Collective)
(redefclass Organization)

;;; TBox: building on dul:Concept ⊑ dul:SocialObject
(redefclass Concept)
(redefclass Role)


;;; Object properties
(redefoproperty associatedWith)
(redefoproperty hasQuality)
(redefoproperty isMemberOf)

(redefoproperty follows)
(redefoproperty directlyFollows)

(redefoproperty precedes)
(redefoproperty directlyPrecedes)

(redefoproperty hasPart)
(redefoproperty hasComponent)

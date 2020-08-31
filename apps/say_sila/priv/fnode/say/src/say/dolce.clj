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
;;;; @copyright 2019-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.dolce
  (:require [say.genie      :refer :all]
            [say.ontology   :refer :all]
            [say.config     :as cfg]
            [say.dllearner  :as dll]
            [say.log        :as log]
            [tawny.repl     :as repl]               ; <= DEBUG
            [tawny.owl      :refer :all]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#")
(def ^:const ONT-FPATH  "resources/KB/DUL.owl")
(def ^:const ONTOLOGY   (load-ontology ONT-IRI ONT-FPATH))


;;; --------------------------------------------------------------------------
(defontology dul
  :iri    ONT-IRI
  :prefix "dul")


;;; --------------------------------------------------------------------------
;;; Create access variables only for the classes we need.
;;;
;;; TBox: building on owl:Thing
(redefclass Entity)
(redefclass Abstract)
(redefclass FormalEntity)
(redefclass Quality)


;;; TBox: building on dul:Object ⊑ dul:Entity
(redef :class Objekt ONT-IRI "Object")      ; Avoid clash with java.lang.Object
(redefclass InformationEntity)
(redefclass InformationObject)

(redefclass Agent)
(redefclass Person)
(redefclass Collective)
(redefclass Organization)

;;; TBox: building on dul:Concept ⊑ dul:SocialObject
(redefclass SocialObject)
(redefclass Collection)
(redefclass Concept)
(redefclass Role)


;;; Object properties
(redefoproperty associatedWith)
(redefoproperty expresses)
(redefoproperty hasRole)
(redefoproperty hasQuality)
(redefoproperty isMemberOf)

(redefoproperty isAbout)
(redefoproperty isReferenceOf)

(redefoproperty follows)
(redefoproperty directlyFollows)

(redefoproperty precedes)
(redefoproperty directlyPrecedes)

(redefoproperty hasPart)
(redefoproperty isPartOf)
(redefoproperty hasComponent)
(redefoproperty isComponentOf)


;;; Tell DL-Learner about our ontology elements
(dll/register-ns)


;;; --------------------------------------------------------------------------
(defn which-mode
  "Reports the configured foundational ontology access mode as one of
  :import (default), :hierarchy or :minimal."
  []
  (cfg/?? :dolce :access :import))



;;; --------------------------------------------------------------------------
(defn access
  "Sets up DOLCE+DnS Ultralite in the caller's ontology according to the
  access method specified in the configuration."
  []
  (let [mode (which-mode)]

    ;; The choice is basically import vs. rebuild
    (if (= mode :import)

      ;; Import the full DUL foundational ontology
      (owl-import dul)

      ;; Recreate the bare-bones minimum from DUL in the client's ontology
      (let[dom->rng (fn [clazz dom rng & args]
                      (apply refine clazz :domain dom :range rng args)
                      clazz)
           ent->ent #(apply dom->rng % Entity Entity %&)]

        ;; The roles we need use the class hierarchy for both the  hierarchy or minimal configurations
        (refine Entity)
        (refine Objekt              :super Entity)
        (refine Agent               :super Objekt)
        (refine Person              :super Agent)

        (refine SocialObject        :super Objekt)
        (refine Collection          :super SocialObject)
        (refine Collective          :super Collection)


        (refine Concept             :super SocialObject)
        (comment refine Role        :super Concept)                 ; TODO: reactivate for Six Americas

        (refine InformationEntity   :super Entity)
        (refine InformationObject   :super InformationEntity)       ; -\ two
        (refine InformationObject   :super SocialObject)            ; -/ parents

        (dom->rng expresses InformationObject SocialObject)         ; Do we want isExpressedBy
        (as-inverse
          (ent->ent hasComponent)
          (ent->ent isComponentOf))

        (as-inverse
          (dom->rng isAbout       InformationObject Entity)
          (dom->rng isReferenceOf Entity InformationObject))

        ;; Recreate what we need for Qualities
        (refine Quality :super Entity)
        (dom->rng hasQuality Entity Quality)

        ;; Properties for linking Entities
        (as-inverse precedes follows)
        (as-inverse directlyPrecedes directlyFollows)

        (refine directlyPrecedes :super precedes)
        (refine directlyFollows  :super follows)

        (doseq [op [precedes directlyPrecedes
                    follows  directlyFollows]]
          (ent->ent op))

        ;; Mark transitive properties as such. (Likage is transitive, direct linkage is not.)
        (doseq [op [precedes
                    follows]]
          (refine op :characteristic :transitive))

        ;; Do we want our part of the DUL hierarchy?
        (when (= mode :hierarchy)

          ;; associatedWith is the top parent property for all DUL object properties
          (ent->ent associatedWith)
          (doseq [op [hasQuality
                      expresses
                      precedes
                      follows]]
            (refine op :super associatedWith))

          (as-inverse
            (ent->ent hasPart   :super associatedWith :characteristic :transitive)
            (ent->ent isPartOf  :super associatedWith :characteristic :transitive))

          (refine hasComponent  :super hasPart)
          (refine isComponentOf :super isPartOf))))))


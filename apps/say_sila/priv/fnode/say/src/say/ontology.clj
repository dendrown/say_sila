;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Ontology utilities
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.ontology
  (:require [say.log         :as log]
            [tawny.owl       :refer :all]
            [tawny.english   :as dl]
            [clojure.java.io :as io])
  (:import  [org.semanticweb.owlapi.model IRI
                                          OWLDataFactory
                                          OWLOntologyID]))


(set! *warn-on-reflection* true)

(defonce Factory (.getOWLDataFactory (owl-ontology-manager)))


;;; --------------------------------------------------------------------------
(defmacro redef
  "
  Creates a Clojure variable which corresponds to an existing class in the
  current ontology.

    otype   : Keyword indicating OWL entity (:class, :dpropert, ...)
    var     : Symbolic name of the variable, and the short name of the class
    iri     : IRI of the ontology, assumed to be a prefix to the IRI of the class
    suffix  : Identifier appended to the IRI prefix to create the full IRI
              (defaults to the string value of var)
  "
  ([otype var]
  `(redef ~otype ~var ~(deref (ns-resolve *ns* 'ONT-IRI))))   ; Caller's namespace

  ([otype var iri]
  `(redef ~otype ~var ~iri (str '~var)))


  ([otype var iri suffix]
  `(def ~var (->owl ~otype ~iri ~suffix))))


(defmacro redefclass     [& args] `(redef :class     ~@args))
(defmacro redefdproperty [& args] `(redef :dproperty ~@args))


;;; --------------------------------------------------------------------------
(defn load-ontology
  "
  Reads an ontology from the file system
  "
  [^String iri
   ^String fpath]
  (let [id  (OWLOntologyID. (IRI/create iri))
        rsc (io/as-file fpath)
        man (owl-ontology-manager)]
    (remove-ontology-maybe id)
    (.loadOntologyFromOntologyDocument man (IRI/create rsc))))


;;; --------------------------------------------------------------------------
(defn ->owl
  "
  Returns the OWL entity for the specified IRI
  "
  ([otype ^String tbox]
  ;; Use a local alias to handle the Factory type hint in one place
  (let [fct     ^OWLDataFactory Factory
        iri     (IRI/create tbox)
        entity  (case otype :class     (.getOWLClass        fct iri)
                            :dproperty (.getOWLDataProperty fct iri))
        change  (owl-import iri)]
    (log/fmt-debug "Importing ~a: ~a" tbox (str change))
    entity))


  ([otype iri tbox]
  (->owl otype (str iri tbox))))



;;; --------------------------------------------------------------------------
(defn get-class
  "
  Returns a class with the specified IRI
  "
  [iri & args]
  (apply ->owl :class iri args))





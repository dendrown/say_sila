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
  (:require [tawny.owl :refer :all]
            [tawny.english   :as dl]
            [clojure.java.io :as io])
  (:import  [org.semanticweb.owlapi.model IRI
                                          OWLDataFactory
                                          OWLOntologyID]))


(set! *warn-on-reflection* true)

;;; --------------------------------------------------------------------------
(defmacro redefclass
  "
  Creates a Clojure variable which corresponds to an existing class in the
  current ontology.

    iri : IRI of the ontology, assumed to be a prefix to the IRI of the class
    var : Symbolic name of the variable, and the short name of the class
  "
  ([var iri]
  `(redefclass ~var ~iri (str '~var)))


  ([var iri suffix]
  `(def ~var (get-class ~iri ~suffix))))



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
(defn get-class
  "
  Returns a class with the specified IRI
  "
  ([iri tbox] (get-class (str iri tbox)))

  ([^String tbox]
  (let [iri     (IRI/create tbox)
        manager (owl-ontology-manager)
        factory (.getOWLDataFactory manager)]
    (.getOWLClass factory iri))))



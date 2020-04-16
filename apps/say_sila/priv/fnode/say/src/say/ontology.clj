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
  (:refer-clojure :exclude [==])
  (:require [say.genie          :refer :all]
            [say.log            :as log]
            [clojure.java.io    :as io]
            [tawny.english      :as dl]
            [tawny.query        :as qry]
            [tawny.owl          :refer :all]
            [clojure.core.logic :refer :all :exclude [annotate is]])
  (:import  [org.semanticweb.owlapi.model   IRI HasIRI
                                            OWLDataFactory
                                            OWLOntologyID]
            [org.semanticweb.owlapi.search EntitySearcher]
            [uk.ac.manchester.cs.owl.owlapi OWLClassImpl]))


(set! *warn-on-reflection* true)

(def ^:const Getters {:class      'getOWLClass
                      :individual 'getOWLNamedIndividual
                      :aproperty  'getOWLAnnotationProperty
                      :dproperty  'getOWLDataProperty
                      :oproperty  'getOWLObjectProperty})

(defonce Factory (.getOWLDataFactory (owl-ontology-manager)))


;;; --------------------------------------------------------------------------
(defmacro ->owl-getter
  "Returns an OWLDataFactory getter function for the OWL entity of the specified
  type. Note that the argument to the returned function should be an IRI object
  (as opposed to a string)."
  [otype]
  `(fn [iri#] (. ^OWLDataFactory Factory ~(Getters otype) iri#)))



;;; --------------------------------------------------------------------------
(defmacro ->owl
  "Returns the OWL entity for the specified IRI."
  ([otype tbox]
  ;; Use a local alias to handle the Factory type hint in one place
  `(let [lookup# (->owl-getter ~otype)
         iri#    (IRI/create ~tbox)
         entity# (lookup# iri#)
         change# (owl-import iri#)]
    (log/fmt-debug "Importing ~a: ~a" ~tbox (str change#))
    entity#))


  ([otype iri tbox]
  `(->owl ~otype (str ~iri ~tbox))))



;;; --------------------------------------------------------------------------
(defmacro redef
  "Creates a Clojure variable which corresponds to an existing class in the
  current ontology.

    otype   : Keyword indicating OWL entity (:class, :dpropert, ...)
    var     : Symbolic name of the variable, and the short name of the class
    iri     : IRI of the ontology, assumed to be a prefix to the IRI of the class
    suffix  : Identifier appended to the IRI prefix to create the full IRI
              (defaults to the string value of var)"
  ([otype var]
  `(redef ~otype ~var ~(deref (ns-resolve *ns* 'ONT-IRI))))   ; Caller's namespace

  ([otype var iri]
  `(redef ~otype ~var ~iri (str '~var)))


  ([otype var iri suffix]
  `(def ~var (->owl ~otype ~iri ~suffix))))


(defmacro redefclass      [& args] `(redef :class      ~@args))
(defmacro redefindividual [& args] `(redef :individual ~@args))
(defmacro redefaproperty  [& args] `(redef :aproperty  ~@args))
(defmacro redefdproperty  [& args] `(redef :dproperty  ~@args))
(defmacro redefoproperty  [& args] `(redef :oproperty  ~@args))



;;; --------------------------------------------------------------------------
(defmacro redefcopy
  "Recreates an OWL object from another Tawny-OWL (ontology) namespace in the
  current (ontology) namespace.  You generally need this instead of defcopy
  if you have no need to refine the object after copying it."
  [obj & args]
  `(do (defcopy ~obj)
       (refine ~obj ~@args)))



;;; --------------------------------------------------------------------------
(defmacro defpun
  "Creates an individual with the same IRI as the specified class.  The class
  must already exist and the variable representing the individual is prepended
  with an « the ». (MyThing refers to the class, and theMyThing to the individual.)"
  [clazz]
  (let [mclazz (vary-meta clazz
                          assoc :tag 'uk.ac.manchester.cs.owl.owlapi.OWLClassImpl)]
    `(def ~(symbol (str "the" (name `~clazz)))
       (individual (. ~mclazz getIRI) :type ~clazz))))



;;; --------------------------------------------------------------------------
(defn load-ontology
  "Reads an ontology from the file system."
  [^String iri
   ^String fpath]
  (let [id  (OWLOntologyID. (IRI/create iri))
        rsc (io/as-file fpath)
        man (owl-ontology-manager)]
    (remove-ontology-maybe id)
    (.loadOntologyFromOntologyDocument man (IRI/create rsc))))



;;; --------------------------------------------------------------------------
(defn iri-fragment
  "Returns the fragment (generally the entity name) portion of an entity's IRI."
  [^HasIRI entity]
  (.getFragment (.getIRI entity)))



;;; --------------------------------------------------------------------------
(defn iri-kv
  "Returns a key-value vector of the form whose key is :iri and whose value is
  the namespace-qualified string form of the IRI for the specified entity."
  [^HasIRI entity]
  [:iri (str (.getIRI entity))])



;;; --------------------------------------------------------------------------
(defn iri-symbol
  "Returns a symbol naming the fragment (generally the entity name) portion
  of an entity's IRI."
  [entity]
  (symbol (iri-fragment entity)))



;;; --------------------------------------------------------------------------
(defn ensure-map
  "Converts an OWL entity into a hashmap if it hasn't already been converted."
  [entity]
  (if (map? entity)
      entity
      (qry/into-map entity)))



;;; --------------------------------------------------------------------------
(defn facto
  "Returns a goal which matches entity on fact."
  [entity fact]
  (qry/matcher (ensure-map entity)
               {:fact [fact]}))



;;; --------------------------------------------------------------------------
(defn check-fact-aux
  "Helper function for check-fact, which returns a hashmap representing the
  value of a property framed as a fact about an individual or nil if no such
  fact exists.

  In addition to the property, this auxillary function requires the unevaluated
  symbolic name of the property (pname) as Tawny-OWL may store this symbol.
  "
  [entity property pname]
  ;(println "*>" property)
  (let [psym  (iri-symbol property)
        piri  (iri-kv property)
        value (first (run 1 [v]
                       (fresh [fact prop]
                         (facto (ensure-map entity) fact)
                         (conso :fact [prop v] fact)
                         (conde
                           [(== prop psym)]         ; Current namespace
                           [(== prop piri)]         ; Imported ontology
                           [(== prop pname)]))))]   ; Another namespace
  (when-not (empty? value)
    (apply hash-map value))))


(defmacro check-fact
  "Returns a hashmap representing the value of an object or data property
  framed as a fact about an individual or nil if no such fact exists."
  [entity property]
  `(check-fact-aux ~entity ~property (symbol (resolve '~property))))



;;; --------------------------------------------------------------------------
(defn get-count
  "Returns the count for the given data property with respect to an entity."
  ([entity dprop]
  (get-count entity dprop 0))


  ([entity dprop default]
  (if-let [cnt (check-fact entity dprop)]
    (Long/parseLong (:literal cnt))
    default)))



;;; --------------------------------------------------------------------------
#env/R false

(defn get-domains
  "Returns a sequence of domains for the specified property in the given
  ontology."
  [ont prop]
  (seq (EntitySearcher/getDomains prop ont)))

#env/R true


;;; --------------------------------------------------------------------------
(defn get-domain
  "Returns the domain of the specified property in the given ontology."
  [ont prop]
  (first (get-domains ont prop)))


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Ontological utilities
;;;;
;;;; @copyright 2018-2020 Dennis Drown et l'Université du Québec à Montréal
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
  (:import  (org.semanticweb.owlapi.model   IRI HasIRI
                                            OWLDataFactory
                                            OWLOntologyID)
            (org.semanticweb.owlapi.profiles OWLProfile
            OWLProfileReport)
            (org.semanticweb.owlapi.search EntitySearcher)
            (uk.ac.manchester.cs.owl.owlapi OWLClassImpl)))


(set! *warn-on-reflection* true)

(def ^:const OWL-Profiles   #{:el :ql :rl :dl})
(def ^:const OWL-Getters    {:class      'getOWLClass
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
  `(fn [iri#] (. ^OWLDataFactory Factory ~(OWL-Getters otype) iri#)))



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
  with «the». (Widget refers to the class, and theWidget to the individual.)"
  [clazz]
  (let [mclazz (vary-meta clazz
                          assoc :tag 'uk.ac.manchester.cs.owl.owlapi.OWLClassImpl)]
    `(def ~(symbol (str "the" (name `~clazz)))
       (individual (. ~mclazz getIRI) :type ~clazz))))



;;; --------------------------------------------------------------------------
(defmacro defoproperty-per
  "Defines an object property per the specified configuration fragment.
  If cfg? is true, this macro acts as defoproperty for prop, creating an
  object property in the ontology associated with the caller's namespace.
  If untrue, the macro defines (once) the variable prop as an alias to
  the specified superclass."
  [cfg? prop & {:keys [super]
                :as   args}]
  ;; TODO: Generalize this macro for other OWL types
  (if (eval cfg?)
      `(defoproperty ~prop ~@(flatten (seq args)))
      `(defonce ~prop ~super)))



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
(defn which-ontology
  "Returns the ontology associated with the specified namespace, defaulting to
  the current (caller's) namespace, or nil if there is no such ontology."
  ([]
  (which-ontology *ns*))

  ([nspace]
  (get @ontology-for-namespace nspace)))



;;; --------------------------------------------------------------------------
(defmacro owl-profile
  "Creates a function that will accept a profile tag and return the corresponding
  OWL profile."
  []
  (let [prof  #(str "org.semanticweb.owlapi.profiles.OWL2" (KEYSTR %) "Profile")
        profs (mapcat #(vector % `(new ~(symbol (prof %))))
                      OWL-Profiles)]
    `(fn [p#]
       (case p#
         ~@profs))))



;;; --------------------------------------------------------------------------
(defn ^OWLProfileReport report
  "Returns an OWL2 QL profile report for the specified ontology."
  ([prof]
  (report prof (which-ontology)))


  ([prof ont]
  (let [chk ^OWLProfile ((owl-profile) prof)]
    (.checkOntology chk ont))))



;;; --------------------------------------------------------------------------
(defn violations
  "Returns true if the specified ontology and its import clojure is within
  the OWL2 QL profile."
  ([prof & opts]
  (let [ont (if (and opts
                     (not (keyword? (first opts))))
                (first opts)
                (which-ontology))
        vs  (.getViolations (report prof ont))]
    ;; Do they just want the types?
    (if (some #{:type} opts)
        (map first (group-by type vs))
        vs))))



;;; --------------------------------------------------------------------------
(defn in-profile?
  "Returns true if the specified ontology and its import clojure is within
  the OWL2 QL profile."
  ([prof]
  (in-profile? prof (which-ontology)))


  ([prof ont]
  (.isInProfile (report prof ont))))



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
  (cond
    (symbol? value)   value                         ; obj. property
    (not-empty value) (apply hash-map value))))     ; data property



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


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Ontological reasoning functionality.
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.infer
  (:require [say.genie          :refer :all]
            [tawny.reasoner     :as rsn])
  (:import  (org.semanticweb.owlapi.model OWLOntology)
            (org.semanticweb.owlapi.reasoner OWLReasoner)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)



;;; --------------------------------------------------------------------------
(defn silence
  "Returns an Atom whose value is a silent reasoner progress monitor."
  []
  (atom rsn/reasoner-progress-monitor-silent))



;;; --------------------------------------------------------------------------
(defmacro with-silence
  "Performs the reasoning operation in the body using a silent reasoner
  progress monitor."
  [& body]
  `(binding [rsn/*reasoner-progress-monitor* (silence)]
     ~@body))



;;; --------------------------------------------------------------------------
(defmacro with-ns-silence
  "Performs the reasoning operation in the body within the specified namespace
  and using a silent reasoner progress monitor."
  [nspace & body]
  `(binding [*ns* (find-ns ~nspace)
             rsn/*reasoner-progress-monitor* (silence)]
     ~@body))



;;; --------------------------------------------------------------------------
(defn unreason
  "Detaches a reasoner from the specified ontology and destroys it (allowing
  precious memory resources to become available once again."
  ([ont]
  (unreason ont (rsn/reasoner-for-ontology ont)))


  ([^OWLOntology ont
    ^OWLReasoner rsnr]
  ;; Make sure HermiT doesn't hoard memory.  Tawny-OWL (as of version 2.0.3) is
  ;; not calling dispose on the HermiT reasoner due to crashiness they've seen.
  (when rsnr
    (.dispose rsnr)
    (rsn/discard-reasoner ont))))


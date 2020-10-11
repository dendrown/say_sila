;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; A community of HermiTs and their ontologies.
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.community
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)


(defonce Community  (atom {}))                      ; TODO: an atom of agents...


;;; --------------------------------------------------------------------------
(defn add!
  "Adds an ontology to the community for the user specified by who."
  [who ont]
  ;; Add a new member, but remember that the ontology is mutable!
  (when who
    (swap! Community conj [who ont])
     ont))



;;; --------------------------------------------------------------------------
(defn fetch
  "Retrieves the onntology for a user, optionally creating one if an ontology
  maker function (onter) is specified.  If no user  is given, the function
  returns a sequence of all ontologies in the community."
  ([]
  ;; No user specified, so return them all!
  (vals @Community))


  ([who]
  ;; Here the ontology should already be in the community (otherwise, nil)
  (get @Community who))


  ([who onter]
  ;; Here we create the ontology if it doesn't already exist
  (get (swap! Community
              #(if (get % who)
                   %                                ; Ontology already there!
                   (conj % [who (onter who)])))     ; New individual ontology
       who)))


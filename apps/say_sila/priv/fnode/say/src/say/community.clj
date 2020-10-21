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
  (:refer-clojure :exclude [load new])
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [clojure.java.io    :as io]
            [tawny.owl          :refer :all]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)



;;; --------------------------------------------------------------------------
(defn new
  "Returns a new community structure."
  []
  (atom {}))



;;; --------------------------------------------------------------------------
(defn add!
  "Adds an ontology to the community for the user specified by who."
  [comm who ont]
  ;; Add a new member, but remember that the ontology is mutable!
  (when who
    (swap! comm conj [who ont])
     ont))



;;; --------------------------------------------------------------------------
(defn fetch
  "Retrieves the onntology for a user, optionally creating one if an ontology
  maker function (onter) is specified.  If no user  is given, the function
  returns a sequence of all ontologies in the community."
  ([comm]
  ;; No user specified, so return them all!
  (vals @comm))


  ([comm who]
  ;; Here the ontology should already be in the community (otherwise, nil)
  (get @comm who))


  ([comm who onter]
  ;; Here we create the ontology if it doesn't already exist
  (get (swap! comm
              #(if (get % who)
                   %                                ; Ontology already there!
                   (conj % [who (onter who)])))     ; New individual ontology
       who)))



;;; --------------------------------------------------------------------------
(defn save
  "Saves the community ontology information."
  [comm dtag]
  (let [tag   (name dtag)
        fstub (strfmt "resources/world/~a" tag)]
    ;; We may have a large set of individual ontologies, give them a subdirectory under world
    (.mkdirs (io/file fstub))
    (run! (fn [[who ont]]
              (save-ontology ont (strfmt "~a/community-~a.~a.owl" fstub tag who) :owl))
          @comm)))



;;; --------------------------------------------------------------------------
(defn size
  "Returns the number of community members."
  [comm]
  (count @comm))



;;; --------------------------------------------------------------------------
(defn zap!
  "Adds an ontology to the community for the user specified by who."
  [comm]
  (reset! comm {}))


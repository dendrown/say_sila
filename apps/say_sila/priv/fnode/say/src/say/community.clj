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


(defonce Community  (agent {}))                     ; An agent of agents


;;; --------------------------------------------------------------------------
(defn add!
  "
  "
  [who ont]
  ;; Add a new member, but remember that the ontology is mutable!
  (when who
    (send Community conj [who ont])
     ont))



;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Handling Ontologies via OWL 
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns sila-weka.owl
  (:require [sila-weka.log :as log]
            [tawny.read    :as rd]))


(set! *warn-on-reflection* true)

(def SAY-SILA-FPATH "resources/ont/say-sila.owl")


;;; --------------------------------------------------------------------------
(defn towl
  "
  Tester for Tawny OWL
  "
  []
  (rd/read :location SAY-SILA-FPATH))




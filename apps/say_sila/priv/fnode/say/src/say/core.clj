;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion Mining and Machine Learning for Climate Change communication
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.core
  (:require [say.sila  :as sila]
            [tawny.owl :as owl]))


;;; --------------------------------------------------------------------------
(defn -main [& args]
  "
  TODO: Re-sibylize the new project structure
  "
  (owl/save-ontology sila/say-sila sila/SAY-SILA-FPATH :owl))

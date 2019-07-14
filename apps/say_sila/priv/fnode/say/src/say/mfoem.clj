;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion Ontology, built on the Ontology of Mental Functions
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.mfoem
  (:require [say.bfo        :as bfo]
            [say.log        :as log]
            [say.ontology   :as ont]
            [tawny.repl     :as repl]               ; <= DEBUG
            [tawny.owl      :as owl]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    bfo/ONT-IRI)
(def ^:const ONT-FPATH  "resources/KB/MFOEM.owl")
(def ^:const ONTOLOGY   (ont/load-ontology ONT-IRI ONT-FPATH))

(owl/defontology bfo
  :iri    ONT-IRI
  :prefix "mfoem")


; Create access variables only for the classes we need
(bfo/redefclass :ogms extended-organism 87)
(bfo/redefclass :mf   human-being       16)


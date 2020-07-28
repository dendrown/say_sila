;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Friend-of-a-friend (foaf) ontology
;;;;
;;;; @copyright 2018-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.foaf
  (:require [say.ontology    :refer :all]
            [say.dolce       :as dul]
            [tawny.owl       :refer :all]
            [tawny.repl      :as repl]              ; <= DEBUG
            [tawny.read      :as rd]
            [clojure.java.io :as io])
  (:import  [org.semanticweb.owlapi.model IRI]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://xmlns.com/foaf/0.1/")
(def ^:const ONT-FPATH  "resources/KB/foaf.owl")
(def ^:const ONTOLOGY   (load-ontology ONT-IRI ONT-FPATH))

(def ^:const Female "FEMALE")
(def ^:const Male   "MALE")

(defontology foaf
  :iri    ONT-IRI
  :prefix "foaf")

; Create access variables only for the foaf classes we need
(redefclass Agent)
(redefclass Person)
(redefclass OnlineAccount)
(redefclass PersonalProfileDocument)

(redefclass Document ONT-IRI)
(redefclass Group    ONT-IRI)


;;; Data properties
(redefdproperty gender)

;;; --------------------------------------------------------------------------
(defn load-fully
  "
  Loads the foaf ontology and (theoretically) makes it available via
  normal tawny.owl functionality.
  "
  []
  (rd/defread foaf
    :prefix   "foaf"
    :iri      ONT-IRI
    :location (IRI/create (io/as-file ONT-FPATH))
    :filter   (partial rd/iri-starts-with-filter ONT-IRI)
    :transform
      (comp rd/stop-characters-transform
            rd/exception-nil-label-transform)))



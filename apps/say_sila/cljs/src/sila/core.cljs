;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; @doc Say-Sila Tweet Emotion Analyzer for Climate Change
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; @end
;;;; -------------------------------------------------------------------------
(ns sila.core
 (:require [clojure.string :as str]
           [clojure.pprint :as prt]))

(enable-console-print!)


;;; --------------------------------------------------------------------------
(defn- get-doc-elm
  "
  Retrieves an element from the document by id.  Note that id is recognized
  in string|keyword|atomic form.
  "
  [id]
  (.getElementById js/document (name id)))


;;; --------------------------------------------------------------------------
(defn- elm-exists?
  "
  Returns true if there exists an element in js/document for the specified id;
  false otherwise.
  "
  [id]
  (some? (get-doc-elm id)))


;;; --------------------------------------------------------------------------
(defn- get-html
  "
  Returns the innerHTML property for the document element with the specified id.
  "
  [id]
  (.-innerHTML (get-doc-elm id)))


;;; --------------------------------------------------------------------------
(defn set-html!
  "
  Sets the innerHTML property for the document element with the specified id.
  "
  [id html]
  (set! (.-innerHTML (get-doc-elm id)) html))


;;; --------------------------------------------------------------------------
(let [csets [{:sets ["A"]     :size 12}
             {:sets ["B"]     :size 12}
             {:sets ["A" "B"] :size  4}]
      jsets (clj->js csets)
      chart (js/venn.VennDiagram)]

  ; Venn Demo
  (-> (.select js/d3 "#venn")
      (.datum jsets)
      (.call chart)))

(set-html! :status "Everyone say Sila!")

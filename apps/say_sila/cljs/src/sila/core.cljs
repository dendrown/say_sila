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
           [clojure.pprint :as prt]
           [clojure.math.combinatorics :as combo]))

(enable-console-print!)

; TODO: Pull configuration details dynamically from the web page,
;       so we don't have to worry about server/client mismatches
(def ^:const COMM-CODES ["TT" "OT" "RT" "TM"])
(def ^:const MIN-P100   10)
(def ^:const MAX-P100   50)


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
(defn- combine-comms
  "
  Creates a sequence of all defined communications
  "
  [comms]
  (let [N  (count comms)
        Ns (range 1 (inc N))]

    (letfn [(combine [n]
              ;(println n)
              (cond
                 (= n 1) (map list comms)
                 (= n N) (list comms)
                 :else   (combo/combinations comms n)))]

      (apply concat (map combine Ns)))))



;;; --------------------------------------------------------------------------
(defn- make-venn-map
  "
  Creates a mapping for use with the Venn library.  The comms input is a
  sequence of one or more communication codes.
  "
  [comms]
  (letfn [(->id [c]
            (str "cnt_40_" (str/join "_" comms)))]

    (let [base {:sets comms
                :size (get-html (->id comms))}]

      ; Identify the singleton sets
      (if (next comms)
          base
          (assoc base :label (first comms))))))



;;; --------------------------------------------------------------------------
(defn- do-venn
  "
  Creates a mapping for use with the Venn library.  The comms input is a
  sequence of one or more communication codes.
  "
  [pct]
  (let [csets (map make-venn-map (combine-comms COMM-CODES))
        jsets (clj->js csets)
        chart (js/venn.VennDiagram)]

    ;(println (combine-comms comms))
    ; Venn Demo
    (-> (.select js/d3 (str "#venn_" pct))
        (.datum jsets)
        (.call chart))))


;;; --------------------------------------------------------------------------
(set-html! :status "Everyone say Sila!")

(doseq [pct (range MIN-P100 (inc MAX-P100) 10)]
  (println "Venn for" pct "%")
  (do-venn pct))

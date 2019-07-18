;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Data collection/preprocessing routings
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.data
  (:require [clojure.string         :as str]
            [net.cgrand.enlive-html :as web])
  (:import  [java.net URL]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const URL-FEMALE-NAMES   "http://www.20000-names.com/female_english_names.htm")
(def ^:const URL-MALE-NAMES     "http://www.20000-names.com/male_english_names.htm")


;;; --------------------------------------------------------------------------
(defn get-dom
  "Pull the document object model from a web resource."
  ([]
  (get-dom URL-FEMALE-NAMES))


  ([url]
  (-> url URL. web/html-resource)))


;;; --------------------------------------------------------------------------
(defn get-content
  "Pulls the :content value from a web structure."
  ([x]
  (first (:content x))))



;;; --------------------------------------------------------------------------
(defn get-content!
  "Pulls the innermost :content value from a nested web structure."
  ([xx]
  (loop [x xx]
    (when-let [c (get-content x)]
      (if (string? c)
           c
           (recur c))))))



;;; --------------------------------------------------------------------------
(defn get-names
  "Pull English male and female names from the web."
  []
  (let [items (-> URL-FEMALE-NAMES
                  get-dom
                  (web/select [:body :ol])
                  first
                  (web/select [:li]))
        names (map #(str/trim (get-content! %)) items)]

    ;; We're got some clean-up to do...
    (filter seq names)))

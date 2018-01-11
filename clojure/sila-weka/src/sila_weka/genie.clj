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
;;;; @copyright 2017 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns sila-weka.genie
  (:import  [java.util  Random]))


(set! *warn-on-reflection* true)

(def RNG (Random. 1))


;;; --------------------------------------------------------------------------
;;; ╻  ╻┏━┓╺┳╸╻┏━╸╻ ╻
;;; ┃  ┃┗━┓ ┃ ┃┣╸ ┗┳┛
;;; ┗━╸╹┗━┛ ╹ ╹╹   ╹
;;; --------------------------------------------------------------------------
(defn ^clojure.lang.PersistentList listify
  "
  If the input argument is not a list, contain it in a list.
  "
  [arg]
  (if (list? arg) arg (list arg)))


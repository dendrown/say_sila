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
;;;; @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.genie
  (:import  [java.util  Random]))


(set! *warn-on-reflection* true)

(def RNG-SEED       (atom 12345))
(def RNG            (Random. @RNG-SEED))


;;; --------------------------------------------------------------------------
;;; ┏━┓┏┓╻┏━╸   ┏━┓┏━╸┏━╸╺┳┓
;;; ┣┳┛┃┗┫┃╺┓╺━╸┗━┓┣╸ ┣╸  ┃┃
;;  ╹┗╸╹ ╹┗━┛   ┗━┛┗━╸┗━╸╺┻┛
;;; --------------------------------------------------------------------------
(defn ^Long rng-seed
  "
  Handle random number generator
  "
  ([]
  (let [seed @RNG-SEED]
    (swap! RNG-SEED inc)
    seed))

  ([seed]
  (swap! RNG-SEED (fn [_] (long seed)))))



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



;;; --------------------------------------------------------------------------
;;; ╺━┓╻┏━┓
;;; ┏━┛┃┣━┛
;;; ┗━╸╹╹
;;; --------------------------------------------------------------------------
(defmacro zip
  "
  Returns a sequence of the zipped up elements from the specified collections.
  "
  [& colls]
  `(map vector ~@colls))


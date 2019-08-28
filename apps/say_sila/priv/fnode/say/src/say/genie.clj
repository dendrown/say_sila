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
  (:require [clojure.pprint :as prt]
            [clojure.string :as str])
  (:import  [java.util  Random]))


(set! *warn-on-reflection* true)

(def RNG-SEED       (atom 12345))
(def RNG            (Random. @RNG-SEED))


;;; --------------------------------------------------------------------------
(defmacro jcall
  "
  Sorta-kinda-not-really similar to apply, but works for java static methods
  with zero or more normal parameters and a series of parameters that involve
  method calls on an object.

  Example usage:
    (jcall some.package/foo 42 [this .doSomething .doSomethingElse .doAnother])

  This macro was originally supposed to be a generic utility, but it didn't
  really end up that way at all, did it?
  "
  ;; Add the parts on in reverse order...
  ([meth [obj & meths]]
  (conj (map #(list % obj) meths) meth))


  ([meth arg1 [obj & meths]]
   (conj (map #(list % obj) meths) arg1 meth)))



;;; --------------------------------------------------------------------------
(defn ^String keystr
  "
  Returns a string representing a keyword without its initial colon ( : ).
  "
  [kw]
  (subs (str kw) 1))



;;; --------------------------------------------------------------------------
(defn ^String KEYSTR
  "
  Returns an uppercased string representing a keyword without its initial
  colon ( : ).
  "
  [kw]
  (str/upper-case (keystr kw)))



;;; --------------------------------------------------------------------------
(defn get-keys
  "Returns a sequence with any keys used in the hashmap hmap that are included
  in the sequence kseq."
  [hmap kseq]
  (keys (select-keys hmap kseq)))



;;; --------------------------------------------------------------------------
(defn ^String strfmt
  "Wrapper for clojure.pprint/cl-format to create strings."
  [text & args]
  (apply prt/cl-format nil text args))



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
(defn ^clojure.lang.PersistentList listify
  "
  If the input argument is not a list, contain it in a list.
  "
  [arg]
  (if (list? arg) arg (list arg)))



;;; --------------------------------------------------------------------------
(defmacro zip
  "
  Returns a sequence of the zipped up elements from the specified collections.
  "
  [& colls]
  `(map vector ~@colls))


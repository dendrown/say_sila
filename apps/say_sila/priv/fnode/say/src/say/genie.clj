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
;;;; @copyright 2017-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.genie
  (:require [clojure.pprint :as prt]
            [clojure.string :as str])
  (:import  [java.util  Random]))


(set! *warn-on-reflection* true)

(def ^:const EPSILON 1e-12)

(defonce NO-OBJS    (into-array Object []))

(def RNG-SPARK      (atom 12345))
(def RNG            (Random. @RNG-SPARK))


;;; --------------------------------------------------------------------------
(defprotocol Equivalency
  "Defines functions for equivalency with relative levels of strictness."
  (equiv? [x y] "Determines if x and y are loosely equivalent."))

(extend-protocol Equivalency
  Object
  (equiv? [x y]
    (= x y))

  String
  (equiv? [x y]
    (if (string? y)
        (apply = (map str/lower-case [x y]))
        (equiv? x (str y))))

  Number
  (equiv? [x y]
    (if (number? y)
        (> EPSILON (Math/abs (- (double x) (double y))))
        (equiv? (str x) y))))



;;; --------------------------------------------------------------------------
(defn reflect!
  "Turns on|off compilation warnings for reflection for the specified form."
  [yn]
  ;; NOTE: the first attempt involved a  binding expression,
  ;;       but the compiler seems to ignore the flag.
  (set! *warn-on-reflection* yn))



;;; --------------------------------------------------------------------------
(defmacro domap
  "Shortcut for (doall (map ...)).  Note that although this macro is somewhat
  more flexible, you should prefer clojure.core/run! when you want to produce
  side-effects and are not interested in the return value."
  [& args]
  `(doall (map ~@args)))



;;; --------------------------------------------------------------------------
(defn fizzbuzz
  "There is no good reason to have this here..."
  ([]
  (fizzbuzz 100))

  ([n & {:as wordmap}]
  (let [gen    #(fn [i] (when (zero? (rem i %1)) (name %2)))
        funs    (map (fn [[word i]] (gen i word))
                     (merge {:fizz 3,
                             :buzz 5}
                            wordmap))
        ->words (fn [i] (apply str (map #(% i) funs)))]
    (apply println
           (map #(if-let [word (not-empty (->words %))] word %)
                 (range 1 (inc n)))))))



;;; --------------------------------------------------------------------------
(defmacro go-let
  "Shortcut for (go (let ...))"
  [& args]
  `(clojure.core.async/go (let ~@args)))



;;; --------------------------------------------------------------------------
(defmacro jcall
  "Sorta-kinda-not-really similar to apply, but works for java static methods
  with zero or more normal parameters and a series of parameters that involve
  method calls on an object.

  Example usage:
    (jcall some.package/foo 42 [this .doSomething .doSomethingElse .doAnother])

  This macro was originally supposed to be a generic utility, but it didn't
  really end up that way at all, did it?"
  ;; Add the parts on in reverse order...
  ([meth [obj & meths]]
  (conj (map #(list % obj) meths) meth))


  ([meth arg1 [obj & meths]]
   (conj (map #(list % obj) meths) arg1 meth)))



;;; --------------------------------------------------------------------------
(defn ^String keystr
  "Returns a string representing a keyword without its initial colon ( : )."
  [kw]
  (subs (str kw) 1))



;;; --------------------------------------------------------------------------
(defn ^String KEYSTR
  "Returns an uppercased string representing a keyword without its initial
  colon ( : )."
  [kw]
  (str/upper-case (keystr kw)))



;;; --------------------------------------------------------------------------
(defn lower-keyword
  "Creates a lower-case keyword from stringable data."
  [s]
  (keyword (str/lower-case (str s))))



;;; --------------------------------------------------------------------------
(defn get-keys
  "Returns a sequence with any keys used in the hashmap hmap that are included
  in the sequence kseq."
  [hmap kseq]
  (keys (select-keys hmap kseq)))



;;; --------------------------------------------------------------------------
(defn optionize
  "Returns a vector pair where the first element is a the first item of the
  opts sequence iff it passes the check function.  In this case the second
  item in the pair will be the rest of the opts sequence.  Otherwise, if the
  check on the first item fails (is falsey), then the first element of the
  return pair is the specified default, and the second item is the full opts
  sequence."
  [check default opts]
  (let [o1 (first opts)]
    (if (check o1)
        [o1 (rest opts)]
        [default opts])))



;;; --------------------------------------------------------------------------
(defn ^String strfmt
  "Wrapper for clojure.pprint/cl-format to create strings."
  [text & args]
  (apply prt/cl-format nil text args))


;;; --------------------------------------------------------------------------
(defn ^Long rng-seed
  "Handle random number generator."
  ([]
  (let [seed @RNG-SPARK]
    (swap! RNG-SPARK inc)
    seed))

  ([seed]
  (swap! RNG-SPARK (fn [_] (long seed)))))



;;; --------------------------------------------------------------------------
(defn ^clojure.lang.PersistentList listify
  "If the input argument is not a list, contain it in a list.  However, nil
  is returned as nil.

  NOTE: you probably want to use seqify instead of this function."
  [arg]
  (when arg
    (if (list? arg) arg (list arg))))



;;; --------------------------------------------------------------------------
(defn ^clojure.lang.PersistentList seqify
  "If the input argument is not a sequence, contain it in one. Note that nil
  returns as nil, and passing a String will return («Hello»), rather than
  a sequence of Character values."
  [arg]
  (seq (cond
         (string? arg)  [arg]
         (seqable? arg) arg
         :else          [arg])))


;;; --------------------------------------------------------------------------
(defn doublify
  "If the input argument is not a double, coerce it to a double.  Anything
  not coercible to a double (including nil) is returned as nil."
  [x]
  (cond
    (number? x) (double  x)
    (string? x) (Double/parseDouble x)
    :else       nil))


;;; --------------------------------------------------------------------------
(defn longify
  "Coerce the input to a long.  Anything not coercible to a long (including nil)
  is returned as nil."
  [x]
  (cond
    (number? x) (long  x)
    (string? x) (long (doublify x))     ; Handle strings with decimals
    :else       nil))


;;; --------------------------------------------------------------------------
(defmacro uncomment
  "Allows for easy debugging, going from (comment ...) to (uncomment ...)
   and back as required during development."
  [& args]
  `(do ~@args))



;;; --------------------------------------------------------------------------
(defn update-keys
  "Maps the specified function across all the elements in a hashmap, updating
  the keys with the value returned by the passed funtion.  This fuction is of
  arity one, accepting the hashmap key.  It has no access to the value."
  [hmap fun]
  (into {} (map (fn [[k v]] [(fun k) v]) hmap)))



;;; --------------------------------------------------------------------------
(defn update-values
  "Maps the specified function across all the values in a hashmap.
  The passed fuction is of arity one, accepting the old hashmap value.
  The caller may optionally specify a sequence of keys, in which case, the
  function is applied only values corresponding to those keys are updated."
  ([hmap fun]
  (into {} (map (fn [[k v]] [k (fun v)]) hmap)))

  ([hmap ks fun]
  (reduce #(assoc %1 %2 (fun (%1 %2))) hmap ks)))


;;; --------------------------------------------------------------------------
(defn update-kv-keys
  "Maps the specified function across all the elements in a hashmap, updating
  the keys with the value returned by the passed funtion.  This fuction is of
  arity two, accepting the old hashmap key and the old value."
  [hmap fun]
  (into {} (map (fn [[k v]] [(fun k v) v]) hmap)))



;;; --------------------------------------------------------------------------
(defn update-kv-values
  "Maps the specified function across all the elements in a hashmap.
  The passed fuction is of arity two, accepting the hashmap key and the old value."
  [hmap fun]
  (into {} (map (fn [[k v]] [k (fun k v)]) hmap)))


;;; --------------------------------------------------------------------------
(defmacro zip
  "Returns a sequence of the zipped up elements from the specified collections."
  [& colls]
  `(map vector ~@colls))


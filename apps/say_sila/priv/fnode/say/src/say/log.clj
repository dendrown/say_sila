;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Logging utilities
;;;;
;;;; @copyright 2017-2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.log
  (:require [clj-time.local :as dts]
            [clojure.pprint :as prt]))

(set! *warn-on-reflection* true)

; Eclipse blocks colour
(def USE-COLOUR (when-not (System/getenv "LEIN_REPL_ACK_PORT") true))
(def ^:const TEXT    "\033[0m")
(def ^:const RED     "\033[0;31m")
(def ^:const GREEN   "\033[0;32m")
(def ^:const BROWN   "\033[0;33m")
(def ^:const LT_GRAY "\033[0;37m")
(def ^:const DK_GRAY "\033[1;30m")
(def ^:const YELLOW  "\033[1;33m")
(def ^:const RED_INV "\033[7;31m")

(defmacro deflevel [lvl prompt colour]
  `(def ^:const ~lvl (if USE-COLOUR (str ~colour ~prompt TEXT) ~prompt)))

(deflevel PANIC  " PANIC:" RED_INV)
(deflevel CRIT   "  CRIT:" RED_INV)
(deflevel ERROR  " ERROR:" RED)
(deflevel WARN   "  WARN:" YELLOW)
(deflevel NOTICE "NOTICE:" GREEN)
(deflevel INFO   "  INFO:" BROWN)
(deflevel DEBUG  " DEBUG:" DK_GRAY)

(def ^:const BRIEF_LENGTH  60)


;;; --------------------------------------------------------------------------
(defn log
  "
  General logger function; prepends date-time stamp and informational level
  "
	[level & args]
  (let [stamp (dts/local-now)
        msg   (conj args            ; Add elements in reverse order
                    level
                    (dts/format-local-time stamp :hour-minute-second-fraction)
                    (dts/format-local-time stamp :date))]
     (apply println msg)))


(defmacro panic  [& args] `(log PANIC  ~@args))
(defmacro crit   [& args] `(log CRIT   ~@args))
(defmacro error  [& args] `(log ERROR  ~@args))
(defmacro warn   [& args] `(log WARN   ~@args))
(defmacro notice [& args] `(log NOTICE ~@args))
(defmacro info   [& args] `(log INFO   ~@args))
(defmacro debug  [& args] `(log DEBUG  ~@args))


;;; --------------------------------------------------------------------------
(defn brief
  "
  Only print the first few characters of a longer string.
  "
  ([text] (brief BRIEF_LENGTH text))

  ([len text]
  (if (<= (count text)  len)
      text
      (str (subs text 0 len) "..."))))


;;; --------------------------------------------------------------------------
(defmacro fmt
  "
  Wrapper for clojure.pprint/cl-format for logging ease.  Output is always
  in string form.
  "
  [text & args]
  `(prt/cl-format nil ~text ~@args))


;;; --------------------------------------------------------------------------
(defmacro <>
  "
  Creates a string of the form «boss<sub>:»
  "
  [boss sub]
  `(str ~boss "<" ~sub ">:"))

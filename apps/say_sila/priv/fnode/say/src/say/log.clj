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
;;;; @copyright 2017-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.log
  (:require [clj-time.local     :as dts]
            [clojure.pprint     :as prt]
            [clojure.stacktrace :as stk]))

(set! *warn-on-reflection* true)

; Eclipse blocks colour :/
(def USE-COLOUR (when-not (System/getenv "LEIN_REPL_ACK_PORT") true))
(def ^:const Text       "\033[0m")
(def ^:const Red        "\033[0;31m")
(def ^:const Green      "\033[0;32m")
(def ^:const Yellow     "\033[0;33m")
(def ^:const Blue       "\033[0;34m")
(def ^:const Magenta    "\033[0;35m")

(def ^:const Lt-Red     "\033[1;31m")
(def ^:const Lt-Yellow  "\033[1;33m")
(def ^:const LT-Magenta "\033[1;35m")

(def ^:const White      "\033[0;97m")

(def ^:const Lt-Gray    "\033[0;37m")
(def ^:const Dk-Gray    "\033[1;30m")

(def ^:const Red-Inv    "\033[7;31m")

(defmacro deflevel [lvl prompt colour]
  `(def ^:const ~lvl (if USE-COLOUR (str ~colour ~prompt Text) ~prompt)))

(deflevel PANIC  " PANIC:" Red-Inv)
(deflevel CRIT   "  CRIT:" Red-Inv)
(deflevel ERROR  " ERROR:" Red)
(deflevel WARN   "  WARN:" Yellow)
(deflevel NOTICE "NOTICE:" Green)
(deflevel INFO   "  INFO:" Blue)
(deflevel DEBUG  " DEBUG:" Dk-Gray)

(def Logger (agent {:count 0}))

;;; --------------------------------------------------------------------------
(defmacro progress
  "TODO: proxy a Print Stream for agent-based logging."
  []
  `(into-array [System/out]))       ; FIXME: !stdout



;;; --------------------------------------------------------------------------
(defn- send-log
  "Define agent alice's logging behaviour."
  [alice msg]
  (let [cnt (:count alice)]
    (apply println msg)
    (assoc alice :count (inc cnt))))


;;; --------------------------------------------------------------------------
(defn wait
  "Flushes the logger agent's output stream so that normal #'println has a
  chance of working."
  []
  (await-for 1000 Logger))



;;; --------------------------------------------------------------------------
(defn log
  "General logger function; prepends date-time stamp and informational level."
  [level & args]
  (let [stamp (dts/local-now)
        msg   (conj args            ; Add elements in reverse order
                    level
                    (dts/format-local-time stamp :hour-minute-second-fraction)
                    (dts/format-local-time stamp :date))]
     (send Logger #(send-log % msg))
     nil))


(defmacro panic  [& args] `(log PANIC  ~@args))
(defmacro crit   [& args] `(log CRIT   ~@args))
(defmacro error  [& args] `(log ERROR  ~@args))
(defmacro warn   [& args] `(log WARN   ~@args))
(defmacro notice [& args] `(log NOTICE ~@args))
(defmacro info   [& args] `(log INFO   ~@args))
(defmacro debug  [& args] `(log DEBUG  ~@args))


;;; --------------------------------------------------------------------------
(defmacro fmt
  "Wrapper for clojure.pprint/cl-format for logging ease.  Output is always
  in string form."
  [text & args]
  `(prt/cl-format nil ~text ~@args))


;;; --------------------------------------------------------------------------
(defn fmt
  "Wrapper for clojure.pprint/cl-format for logging ease.  Output is always
  in string form."
  [text & args]
  (apply prt/cl-format nil text args))


;;; --------------------------------------------------------------------------
(defn fmt!
  "Wrapper for clojure.pprint/cl-format for logging ease.  The results always
  go to standard output."
  [text & args]
  (apply prt/cl-format true text args))


(defmacro fmt-panic  [text & args] `(log PANIC  (fmt ~text ~@args)))
(defmacro fmt-crit   [text & args] `(log CRIT   (fmt ~text ~@args)))
(defmacro fmt-error  [text & args] `(log ERROR  (fmt ~text ~@args)))
(defmacro fmt-warn   [text & args] `(log WARN   (fmt ~text ~@args)))
(defmacro fmt-notice [text & args] `(log NOTICE (fmt ~text ~@args)))
(defmacro fmt-info   [text & args] `(log INFO   (fmt ~text ~@args)))
(defmacro fmt-debug  [text & args] `(log DEBUG  (fmt ~text ~@args)))


;;; --------------------------------------------------------------------------
(defmacro <>
  "Creates a string of the form «boss<sub>:»"
  [boss sub]
  `(str ~boss "<" ~sub ">:"))


;;; --------------------------------------------------------------------------
(defn fail
  "Catch-all error logger for exceptions that excepted."
  [^Exception ex
   ^String    msg
   &          opts]
  (error (str msg ":") (.getMessage ex))
  (when (some #{:stack} opts)
    (stk/print-stack-trace ex)))

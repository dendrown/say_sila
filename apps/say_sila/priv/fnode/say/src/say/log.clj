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

;;; Eclipse blocks colour :/
(def USE-COLOUR (when-not (System/getenv "LEIN_REPL_ACK_PORT") true))

;;; Standard xterm colours
;;; @ref https://misc.flogisoft.com/bash/tip_colors_and_formatting
(def ^:const Text       "\033[0m")
(def ^:const Bright     "\033[1m")
(def ^:const Dim        "\033[2m")
(def ^:const Underline  "\033[4m")

(def ^:const Red        "\033[0;31m")
(def ^:const Green      "\033[0;32m")
(def ^:const Yellow     "\033[0;33m")
(def ^:const Blue       "\033[0;34m")
(def ^:const Magenta    "\033[0;35m")
(def ^:const Cyan       "\033[0;36m")

(def ^:const Lt-Red     "\033[1;31m")
(def ^:const Lt-Green   "\033[1;92m")
(def ^:const Lt-Yellow  "\033[1;33m")
(def ^:const Lt-Blue    "\033[1;94m")
(def ^:const Lt-Magenta "\033[1;35m")
(def ^:const Lt-Cyan    "\033[1;36m")

(def ^:const White      "\033[0;97m")

(def ^:const Lt-Gray    "\033[0;37m")
(def ^:const Dk-Gray    "\033[1;30m")

(def ^:const Red-Inv    "\033[7;31m")

;;; Selections from the 256-colour xterm palette
;;; @ref https://jonasjacek.github.io/colors/
(def ^:const Green4         "\033[38;5;28m")
(def ^:const Green3         "\033[38;5;34m")
(def ^:const Turquoise2     "\033[38;5;45m")
(def ^:const SpringGreen1   "\033[38;5;48m")
(def ^:const Cyan1          "\033[38;5;51m")
(def ^:const Dk-Violet      "\033[38;5;128m")
(def ^:const Magenta3       "\033[38;5;164m")
(def ^:const Orange3        "\033[38;5;172m")
(def ^:const Red1           "\033[38;5;196m")
(def ^:const Magenta2       "\033[38;5;200m")
(def ^:const Magenta1       "\033[38;5;201m")
(def ^:const Lt-Salmon1     "\033[38;5;216m")
(def ^:const Gold1          "\033[38;5;220m")
(def ^:const Yellow1        "\033[38;5;226m")

;;; --------------------------------------------------------------------------
(defmacro deflevel [lvl prompt colour]
  `(def ^:const ~lvl (if USE-COLOUR (str ~colour ~prompt Text) ~prompt)))

(deflevel PANIC  " PANIC:" Red-Inv)
(deflevel CRIT   "  CRIT:" Red-Inv)
(deflevel ERROR  " ERROR:" Lt-Red)
(deflevel WARN   "  WARN:" Lt-Yellow)
(deflevel NOTICE "NOTICE:" Lt-Green)
(deflevel INFO   "  INFO:" Lt-Blue)
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


;;; --------------------------------------------------------------------------
(defmacro st
  "Runs the specified expression in a try/catch loop that gives a stack
  trace if things go badly."
  [& args]
  `(try (do ~@args) (catch Exception ex# (fail ex# "FAIL:" :stack))))


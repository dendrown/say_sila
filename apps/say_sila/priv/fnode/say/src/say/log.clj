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
  (:require [clj-time.local :as dts]))

(set! *warn-on-reflection* true)

;;; --------------------------------------------------------------------------
(defmacro log-msg
  "
  General logger function; prepends date-time stamp and informational level
  "
	[level & msg-args]
  `(let [dts# (dts/local-now)]
     (println (dts/format-local-time dts# :date)
              (dts/format-local-time dts# :hour-minute-second-fraction)
              ~level ~@msg-args)))

(defmacro panic  [& msg-args] `(log-msg "PANIC:"  ~@msg-args))
(defmacro crit   [& msg-args] `(log-msg "CRIT:"   ~@msg-args))
(defmacro error  [& msg-args] `(log-msg "ERROR:"  ~@msg-args))
(defmacro warn   [& msg-args] `(log-msg "WARN:"   ~@msg-args))
(defmacro notice [& msg-args] `(log-msg "NOTICE:" ~@msg-args))
(defmacro info   [& msg-args] `(log-msg "INFO:"   ~@msg-args))
(defmacro debug  [& msg-args] `(log-msg "DEBUG:"  ~@msg-args))



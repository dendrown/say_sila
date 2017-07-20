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
(ns sila-weka.core
  (:import [com.ericsson.otp.erlang OtpNode]))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
;;; ┏━┓╺┳╸┏━┓┏━┓╺┳╸
;;; ┗━┓ ┃ ┣━┫┣┳┛ ┃
;;; ┗━┛ ╹ ╹ ╹╹┗╸ ╹
;;; --------------------------------------------------------------------------
(defn start
  "
  Starts up an Erlang jInterface process for communication with Erlang sila
  "
  []
  (println "Starting OTP process")
  (let [otp-node (OtpNode."weka")
        opt-mbox (.createMbox  otp-node "weka")]
    (.setCookie otp-node "say_sila_uqam_00")
    ;; Quick check that things are connected
    (for [node (.getNames otp-node)]
      (println "NODE: " node))
    (.close otp-node))
    'ok)

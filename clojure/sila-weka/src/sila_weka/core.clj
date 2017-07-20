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
  (:import [com.ericsson.otp.erlang OtpErlangTuple OtpMbox OtpNode]))

(set! *warn-on-reflection* true)

(def ^:const +ERLANG-COOKIE+ "say_sila_uqam_00")


;;; --------------------------------------------------------------------------
;;; ┏━┓╺┳╸┏━┓   ╻  ┏━┓┏━┓┏━┓
;;; ┃ ┃ ┃ ┣━┛╺━╸┃  ┃ ┃┃ ┃┣━┛
;;; ┗━┛ ╹ ╹     ┗━╸┗━┛┗━┛╹
;;; --------------------------------------------------------------------------
(defn otp-loop
  "
  Recursive receive loop for an OTP process
  "
  ([node mbox] (otp-loop node mbox true))

  ([node mbox cont?]
    (when cont?
      (let [tuple    #^OtpErlangTuple (.receive #^OtpMbox mbox)
            sender   (.elementAt tuple 0)
            dispatch (.elementAt tuple 1)
            message  (.elementAt tuple 2)]
        (println sender "<" dispatch ">: " message)
        (recur node mbox (.equals "bye" (.toString message)))))))


;;; --------------------------------------------------------------------------
;;; ┏━┓╺┳╸┏━┓┏━┓╺┳╸
;;; ┗━┓ ┃ ┣━┫┣┳┛ ┃
;;; ┗━┛ ╹ ╹ ╹╹┗╸ ╹
;;; --------------------------------------------------------------------------
(defn start
  "
  Starts up an Erlang jInterface process for communication with Erlang sila
  "
  ([] (start "clojure"))

  ([name]
    (println "Starting OTP process")
    (let [node (OtpNode. name)
          mbox (.createMbox  node "weka")]
      (.setCookie node +ERLANG-COOKIE+)
      (otp-loop node mbox)
      (.close node))
    'ok))

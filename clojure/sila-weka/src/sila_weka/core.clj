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
(def ^:const +RECV-TIMEOUT+  300000)


;;; --------------------------------------------------------------------------
;;; ┏┳┓┏━┓╻┏ ┏━╸   ┏┓╻┏━┓╺┳┓┏━╸
;;; ┃┃┃┣━┫┣┻┓┣╸ ╺━╸┃┗┫┃ ┃ ┃┃┣╸
;;; ╹ ╹╹ ╹╹ ╹┗━╸   ╹ ╹┗━┛╺┻┛┗━╸
;;; --------------------------------------------------------------------------
(defn #^OtpNode make-node
  "
  Creates and initializes a jInterface OTP node
  "
  [name]
  (let [node (OtpNode. name)]
    (.setCookie node +ERLANG-COOKIE+)
    node))



;;; --------------------------------------------------------------------------
;;; ┏━┓┏━┓┏━┓┏━┓┏━╸   ┏┳┓┏━┓┏━╸
;;; ┣━┛┣━┫┣┳┛┗━┓┣╸ ╺━╸┃┃┃┗━┓┃╺┓
;;; ╹  ╹ ╹╹┗╸┗━┛┗━╸   ╹ ╹┗━┛┗━┛
;;; --------------------------------------------------------------------------
(defn parse-msg
  "
  Breaks apart an incoming message into [sender dispatch message].
  This function can be much more lispy...!
  "
  [#^OtpErlangTuple tuple]
  (if (some? tuple)
      [(.elementAt tuple 0)
       (.elementAt tuple 1)
       (.elementAt tuple 2)]
      ["TIMEOUT" "none" "bye"]))


;;; --------------------------------------------------------------------------
;;; ┏━┓╺┳╸┏━┓   ╻  ┏━┓┏━┓┏━┓
;;; ┃ ┃ ┃ ┣━┛╺━╸┃  ┃ ┃┃ ┃┣━┛
;;; ┗━┛ ╹ ╹     ┗━╸┗━┛┗━┛╹
;;; --------------------------------------------------------------------------
(defn otp-loop
  "
  Recursive receive loop for an OTP process
  "
  ([node mbox] (otp-loop node mbox false))

  ([node mbox quit?]
    (when-not quit?
      (let [tuple       #^OtpErlangTuple (.receive #^OtpMbox mbox +RECV-TIMEOUT+)
            [sender
             dispatch
             message]   (parse-msg tuple)]
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
    (let [node (make-node name)
          mbox (.createMbox  node "weka")]
      (println "Started: " (.node    node))
      (println "Cookie : " (.cookie  node))
      (println "Mailbox: " (.getName mbox))
      (println "Pinging: " (.ping node "sila@chiron" 2000))
      (otp-loop node mbox)
      (.close node))
    'ok))

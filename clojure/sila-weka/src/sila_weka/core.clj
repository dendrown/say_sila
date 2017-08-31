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
  (:require [sila-weka.weka :as weka])
  (:require [sila-weka.log  :as log])
  (:import  [com.ericsson.otp.erlang OtpErlangAtom
                                     OtpErlangObject
                                     OtpErlangPid
                                     OtpErlangTuple
                                     OtpErlangString
                                     OtpMbox
                                     OtpNode]))

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
;;; ╺┳┓╻┏━┓┏━┓┏━┓╺┳╸┏━╸╻ ╻
;;;  ┃┃┃┗━┓┣━┛┣━┫ ┃ ┃  ┣━┫
;;; ╺┻┛╹┗━┛╹  ╹ ╹ ╹ ┗━╸╹ ╹
;;; --------------------------------------------------------------------------
(defmulti dispatch
  "
  Process commands coming in from Sila Erlang nodes
  "
  :cmd)

(defmethod dispatch "embed" [msg]
  (let [fpath (.stringValue #^OtpErlangString (:arg msg))]
  (log/info "Filter/EMBED:" fpath)
  (weka/filter-arff fpath :embed)
  (log/info "Filter/EMBED:" fpath "OK")))


(defmethod dispatch "lex" [msg]
  (let [fpath (.stringValue #^OtpErlangString (:arg msg))]
  (log/info "Filter/LEX:" fpath)
  (weka/filter-arff fpath :lex)
  (log/info "Filter/LEX:" fpath "OK")))


(defmethod dispatch "test" [msg]
  (log/info "TEST:" (:arg msg)))


(defmethod dispatch "bye" [msg]
  (log/notice "Time to say « adieu »")
  :quit)


(defmethod dispatch :default [msg]
  (log/warn "HUH? Unknown command:" (:cmd msg)))



;;; --------------------------------------------------------------------------
;;; ┏━┓┏━┓┏━┓┏━┓┏━╸   ┏┳┓┏━┓┏━╸
;;; ┣━┛┣━┫┣┳┛┗━┓┣╸ ╺━╸┃┃┃┗━┓┃╺┓
;;; ╹  ╹ ╹╹┗╸┗━┛┗━╸   ╹ ╹┗━┛┗━┛
;;; --------------------------------------------------------------------------
(defn parse-msg
  "
  Breaks apart an incoming message into [sender command args].
  This function can be much more lispy...!
  "
  [#^OtpErlangTuple tuple]
  (if (some? tuple)
    (let [arity (.arity tuple)]
      ; Note that we're assuming datatypes: pid() atom() term()
      {:src (if (>= arity 1) (.node      #^OtpErlangPid  (.elementAt tuple 0)) nil)
       :cmd (if (>= arity 2) (.atomValue #^OtpErlangAtom (.elementAt tuple 1)) nil)
       :arg (if (>= arity 3)                             (.elementAt tuple 2)  nil)})
      {:src "TIMEOUT" :cmd "bye" :arg nil}))



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

  ([node mbox quitter]
    (when-not (identical? quitter :quit)
      (let [tuple #^OtpErlangTuple (.receive #^OtpMbox mbox +RECV-TIMEOUT+)
            msg   (parse-msg tuple)]
        (log/debug (:src msg) "<" (:cmd msg) ">:" (:arg msg))
        (recur node mbox (dispatch msg))))))



;;; --------------------------------------------------------------------------
;;; ┏━┓╺┳╸┏━┓┏━┓╺┳╸
;;; ┗━┓ ┃ ┣━┫┣┳┛ ┃
;;; ┗━┛ ╹ ╹ ╹╹┗╸ ╹
;;; --------------------------------------------------------------------------
(defn start
  "
  Starts up an Erlang jInterface process for communication with Erlang sila
  "
  ([] (start "jvm"))

  ([name]
    (let [node (make-node name)
          mbox (.createMbox  node "weka")]
      (log/notice "Started: " (.node    node))
      (log/notice "Mailbox: " (.getName mbox))
      (log/debug "Cookie : " (.cookie  node))
      (log/debug "Pinging: " (.ping node "sila@chiron" 2000))
      (otp-loop node mbox)
      (.close node))
    'ok))

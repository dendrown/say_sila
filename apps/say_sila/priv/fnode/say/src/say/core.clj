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
(ns say.core
  (:require [say.sila :as sila]
            [say.weka :as weka]
            [say.log  :as log]
            [clojure.data.json :as json])
  (:import  [com.ericsson.otp.erlang OtpErlangAtom
                                     OtpErlangBinary
                                     OtpErlangList
                                     OtpErlangMap
                                     OtpErlangObject
                                     OtpErlangPid
                                     OtpErlangRef
                                     OtpErlangTuple
                                     OtpErlangString
                                     OtpMbox
                                     OtpNode]))

(set! *warn-on-reflection* true)

(def ^:const +ERLANG-COOKIE+ "say_sila_uqam_00")

(def +to-sila+ (agent {:cnt 0}))       ; TODO: Serialize responses to Erlang



;;; --------------------------------------------------------------------------
;;; ┏┳┓┏━┓╻┏ ┏━╸   ┏┓╻┏━┓╺┳┓┏━╸
;;; ┃┃┃┣━┫┣┻┓┣╸ ╺━╸┃┗┫┃ ┃ ┃┃┣╸
;;; ╹ ╹╹ ╹╹ ╹┗━╸   ╹ ╹┗━┛╺┻┛┗━╸
;;; --------------------------------------------------------------------------
(defn- ^OtpNode make-node
  "
  Creates and initializes a jInterface OTP node.
  "
  [name]
  (let [node (OtpNode. name)]
    (.setCookie node +ERLANG-COOKIE+)
    node))



;;; --------------------------------------------------------------------------
;;; ┏┳┓┏━┓┏━┓    ┏━┓╺┳╸┏━┓
;;; ┃┃┃┣━┫┣━┛╺━╸➤┃ ┃ ┃ ┣━┛
;;; ╹ ╹╹ ╹╹      ┗━┛ ╹ ╹
;;; --------------------------------------------------------------------------
(defn map->otp
  "
  Our standard communication to Erlang Sila is {self(), atom(), map()}.
  This function converts a clojure map in the form {:keyword «string»}
  to an Erlang map to handle that third element in response tuples.

  NOTE: This function currently works only on shallow maps.
  "
  [clj-map]
  (let [clj-keys (keys clj-map)
        clj-vals (vals clj-map)
        otp-keys (into-array OtpErlangObject (map #(OtpErlangAtom. (name %))    clj-keys))
        otp-vals (into-array OtpErlangObject (map #(OtpErlangString. ^String %) clj-vals))]
    (OtpErlangMap. otp-keys otp-vals)))




;;; --------------------------------------------------------------------------
;;; ┏━┓┏┓╻┏━┓╻ ╻┏━╸┏━┓   ┏━┓╻╻  ┏━┓
;;; ┣━┫┃┗┫┗━┓┃╻┃┣╸ ┣┳┛╺━╸┗━┓┃┃  ┣━┫
;;; ╹ ╹╹ ╹┗━┛┗┻┛┗━╸╹┗╸   ┗━┛╹┗━╸╹ ╹
;;; --------------------------------------------------------------------------
(defn- answer-sila
  "
  Send a response, with an optional argument, to the Erlang Sila server.
  "
  ([req rsp-key]
    (answer-sila req rsp-key nil))


  ([req rsp-key rsp-arg]
  (let [mbox    ^OtpMbox      (:mbox req)
        from    (.self mbox)
        ref     (:ref req)
        rsp     (name rsp-key)
        otp-rsp (OtpErlangAtom. rsp)
        elms    (if rsp-arg   [from ref otp-rsp rsp-arg]
                              [from ref otp-rsp])
        otp-msg (OtpErlangTuple. ^"[Lcom.ericsson.otp.erlang.OtpErlangObject;"
                                 (into-array OtpErlangObject elms))]
    (log/debug "SILA<" rsp ">:" rsp-arg)
    (.send mbox ^OtpErlangPid  (:pid req)
                                 otp-msg))))


;;; --------------------------------------------------------------------------
;;; ╺┳┓┏━┓   ╻ ╻┏━╸╻┏ ┏━┓
;;;  ┃┃┃ ┃╺━╸┃╻┃┣╸ ┣┻┓┣━┫
;;; ╺┻┛┗━┛   ┗┻┛┗━╸╹ ╹╹ ╹
;;; --------------------------------------------------------------------------
(defmacro do-weka
  "
  Template for processing a command with Weka and then sending the result
  back to sila.
  "
  [cmd msg weka-fn weka-arg]

  `(let [fpath# (.stringValue ^OtpErlangString (:arg ~msg))]
  (log/info "->> weka<" ~cmd ">:" fpath#)
  (future
    ; NOTE: At this point the weka call is strict on its form.
    ;       We probably want to generalize it.
    (let [rsp# (~weka-fn fpath# ~weka-arg)]
      (log/info "<<- weka<" ~cmd ">" rsp# "[OK]")
      (answer-sila ~msg (keyword ~cmd) (map->otp rsp#))))))



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

(defmethod dispatch "emote"   [msg] (do-weka 'emote   msg weka/filter-arff weka/+EMOTE-FILTER+))
(defmethod dispatch "dic9315" [msg] (do-weka 'dic9315 msg weka/filter-arff '(:embed :bws)))

(defmethod dispatch "sila" [msg]
  (future
      (let [json (String. (.binaryValue ^OtpErlangBinary (:arg msg)))
            arg  (json/read-str json
                                :key-fn keyword)]
        ; No response sent back to Erlang
        (sila/do-command arg))))


(defmethod dispatch "embed" [msg]
  (let [fpath (.stringValue ^OtpErlangString (:arg msg))]
    (log/info "Filter/EMBED:" fpath)
    (weka/filter-arff fpath :embed)
    (log/info "Filter/EMBED:" fpath "[OK]")))


(defmethod dispatch "lex" [msg]
  (let [fpath (.stringValue ^OtpErlangString (:arg msg))]
    (log/info "Filter/LEX:" fpath)
    (weka/filter-arff fpath :lex)
    (log/info "Filter/LEX:" fpath "[OK]")))


(defmethod dispatch "ping" [msg]
  (log/info "ping from" (:src msg) ":" (:arg msg))
  (answer-sila msg :pong))


(defmethod dispatch "bye" [msg]
  (log/notice "Time to say « adieu »")
  (answer-sila msg :bye)
  :quit)


(defmethod dispatch :default [msg]
  (log/warn "HUH? Unknown command:" (:cmd msg)))



;;; --------------------------------------------------------------------------
;;; ┏━┓┏━┓┏━┓┏━┓┏━╸   ┏┳┓┏━┓┏━╸
;;; ┣━┛┣━┫┣┳┛┗━┓┣╸ ╺━╸┃┃┃┗━┓┃╺┓
;;; ╹  ╹ ╹╹┗╸┗━┛┗━╸   ╹ ╹┗━┛┗━┛
;;; --------------------------------------------------------------------------
(defn- parse-msg
  "
  Breaks apart an incoming message into [sender command args].
  This function can be much more lispy...!
  "
  [^OtpErlangTuple tuple]
  (if (some? tuple)
    (let [pid  ^OtpErlangPid (.elementAt tuple 0)
          node  (.node pid)
          arity (.arity tuple)]
      ; Note that we're assuming datatypes: pid() ref() atom() term()
      {:pid pid
       :src node
       :ref (when (>= arity 2)             ^OtpErlangRef  (.elementAt tuple 1))
       :cmd (when (>= arity 3) (.atomValue ^OtpErlangAtom (.elementAt tuple 2)))
       :arg (when (>= arity 4)                            (.elementAt tuple 3))})
      {:src "TIMEOUT" :cmd "bye" :arg nil}))



;;; --------------------------------------------------------------------------
;;; ┏━┓╺┳╸┏━┓   ╻  ┏━┓┏━┓┏━┓
;;; ┃ ┃ ┃ ┣━┛╺━╸┃  ┃ ┃┃ ┃┣━┛
;;; ┗━┛ ╹ ╹     ┗━╸┗━┛┗━┛╹
;;; --------------------------------------------------------------------------
(defn- otp-loop
  "
  Recursive receive loop for an OTP process
  "
  ([node mbox] (otp-loop node mbox false))

  ([node mbox quitter]
    (when-not (identical? quitter :quit)

      ; Block, waiting on something to do
      ;
      ;(log/info "Waiting on SILA command...")
      (let [tuple ^OtpErlangTuple (.receive ^OtpMbox mbox)
            msg   (parse-msg tuple)]

        ;(log/debug (:src msg) "<" (:cmd msg) ">:" (:arg msg))
        (recur node mbox (dispatch (conj msg {:mbox mbox})))))))



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
          mbox (.createMbox  node "say")]
      (log/notice "Started:" (.node    node))
      (log/notice "Mailbox:" (.getName mbox))
      (log/debug  "Cookie :" (.cookie  node))
      (log/debug  "Pinging:" (.ping node "gw@chiron" 2000)) ; FIXME: !hard-coded

      ; FIXME:  The ontology functionality in the say.sila namespace requires
      ;         us to be in say.sila so it can create individuals as variables
      ;         in that name-space. Tawny.owl macros are picky and fragile :(
      (ns say.sila)

      ; Receive and process messages from Erlang
      (otp-loop node mbox)
      (.close node)

      ; Put us back where we're supposed to be
      (ns say.core)
    'ok)))

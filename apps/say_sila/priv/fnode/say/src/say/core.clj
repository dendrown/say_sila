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
(ns say.core
  (:refer-clojure :exclude [==])
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.check          :as chk]
            [say.data           :as data]
            [say.log            :as log]
            [say.hierarchy      :as inua]
           ;[say.optimize       :as ga]                     ; Use GraalVM for Jenetics
            [say.sila           :as sila]
            [say.senti          :as senti]
            [weka.core          :as weka]
            [weka.dataset       :as dset]
            [weka.senti         :as ws]
           ;[clojure.core.logic :as l :refer [run* ==]]     ; <= debug
            [tawny.repl         :as repl]
            [clojure.data.json  :as json]
            [clojure.string     :as str]
            [tawny.owl          :as owl])
  (:import  [com.ericsson.otp.erlang OtpErlangAtom
                                     OtpErlangBinary
                                     OtpErlangDouble
                                     OtpErlangList
                                     OtpErlangLong
                                     OtpErlangMap
                                     OtpErlangObject
                                     OtpErlangPid
                                     OtpErlangRef
                                     OtpErlangTuple
                                     OtpErlangString
                                     OtpMbox
                                     OtpNode]))

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
(defn- ^OtpNode make-node
  "Creates and initializes a jInterface OTP node."
  [sname cookie]
  (doto (OtpNode. sname)
        (.setCookie cookie)))



;;; --------------------------------------------------------------------------
(defn ->otp
  "Our standard communication to Erlang Sibyl is {self(), atom(), map()}.
  This function converts a clojure map in the form {:keyword «string»}
  to an Erlang map to handle that third element in response tuples."
  [arg & opts]
  (let [keyer (if (= :binary (cfg/?? :erlang :keyword))
                  #(OtpErlangBinary. (.getBytes (name %)))
                  #(OtpErlangAtom.   (name %)))]

    (letfn [(==>otp [x]
              (cond
                (keyword? x) (keyer x)
                (symbol?  x) (OtpErlangBinary. (name x))
                (float?   x) (OtpErlangDouble. (double x))
                (number?  x) (OtpErlangLong.   (long x))
                (string?  x) (OtpErlangBinary. (.getBytes ^String x))
                (class?   x) (OtpErlangBinary. (.getBytes ^String (second (str/split (str x) #" "))))
                (map?     x) (map->otp x)
                (seqable? x) (OtpErlangList.   ^"[Lcom.ericsson.otp.erlang.OtpErlangObject;"
                                               (into-array OtpErlangObject (map ==>otp x)))
                :else        (OtpErlangAtom.   "undefined")))

            (map->otp [x]
              (let [clj-keys (keys x)
                    clj-vals (vals x)
                    otp-keys (into-array OtpErlangObject (map ==>otp clj-keys))
                    otp-vals (into-array OtpErlangObject (map ==>otp clj-vals))]
                 (OtpErlangMap. otp-keys otp-vals)))]

      (try
        (if (map? arg)
          ;;
          ;; A map is special for one of two reasons...
          (if (some #{:json} opts)
            (==>otp (json/write-str arg))   ;; The caller wants it converted to JSON, or
            (map->otp arg))                 ;; We must handle keys and values separately

          ;; Singleton and simple-series datatypes can just call the converter
          (==>otp arg))

        (catch Exception ex (log/fail ex "Cannot create Erlang structure" :stack))))))



;;; --------------------------------------------------------------------------
(defn- answer-sila
  "Send a response, with an optional argument, to the Erlang Sila server."
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
    (log/debug "SILA<" rsp ">:" (str rsp-arg))
    (.send mbox ^OtpErlangPid  (:pid req)
                                 otp-msg))))


;;; --------------------------------------------------------------------------
(defmacro do-weka
  "Template for processing a command with Weka and then sending the result
  back to sila."
  [msg fun & parms]
  `(let [{cmd# :cmd
          arg# :arg} ~msg]
    (log/info "->> weka<" cmd# ">:" (str arg#))

    (go-let [wfun# (ns-resolve 'weka.tweet (symbol '~fun))
            rsp#  (apply wfun# arg# '~parms)]
        (log/info "<<- weka<" cmd# ">" rsp# "[OK]")
        (answer-sila ~msg (keyword cmd#) (->otp rsp#)))))



;;; --------------------------------------------------------------------------
(defmulti dispatch
  "Process commands coming in from Sila Erlang nodes.

  TODO: Refactor the older weka/sila dispatch methods so requests go through
        the default method."
  :cmd)

(defmethod dispatch "emote"   [msg] (do-weka msg emote-arff))
(defmethod dispatch "dic9315" [msg] (do-weka msg filter-arff '(:embed :bws)))
(defmethod dispatch "regress" [msg] (do-weka msg regress))


(defmethod dispatch "sila" [msg]
  ;; Handle updates to the say-sila ontology (no response back to Erlang)
  (sila/execute (:arg msg)))


(defmethod dispatch "ping" [msg]
  (log/info "ping from" (:src msg))
  (answer-sila msg :pong))


(defmethod dispatch "bye" [msg]
  (log/notice "Time to say « adieu »")
  (answer-sila msg :bye)
  :quit)


(defmethod dispatch :default [{:as   msg
                               :keys [cmd arg]}]
  ;; Sibyl/Erlang interface. Commands have the form 'scope_function'
  (go-let [[fun
            scope]  (reverse (str/split cmd #"_" 2))
           qfun     (ns-resolve (symbol (str "say." (if scope scope "core")))
                                (symbol fun))
           rsp      (apply qfun (listify arg))]
    ;; Return results to Erlang
    (answer-sila msg (keyword cmd) (->otp rsp))))



;;; --------------------------------------------------------------------------
(defprotocol OtpParser
  "Parsing functionality for messages coming in from Erlang/OTP"
  (parse-arg [arg]  "The optional argument (fourth) element of an incoming message
                     from Erlang may be either a simple string or a JSON formatted map."))


(extend-protocol OtpParser
  OtpErlangAtom
  (parse-arg [arg]
    (keyword (.atomValue arg)))


  OtpErlangString
  (parse-arg [arg]
    (.stringValue arg))


  OtpErlangBinary
  (parse-arg [arg]
    (-> (.binaryValue arg)
        (String.)
        (json/read-str :key-fn keyword))))



;;; --------------------------------------------------------------------------
(defn- parse-msg
  "Breaks apart an incoming message into [sender command args].
  This function can be much more lispy...!"
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
       :arg (when (>= arity 4) (parse-arg                 (.elementAt tuple 3)))})
      {:src "TIMEOUT" :cmd "bye" :arg nil}))



;;; --------------------------------------------------------------------------
(defn- otp-loop
  "Recursive receive loop for an OTP process."
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
(defn start
  "Starts up an Erlang jInterface process for communication with Erlang sila."
  ([]
  (let [ecfg  (cfg/? :erlang)
        node  (make-node (ecfg :sname)
                         (ecfg :cookie))
        mbox  (.createMbox  node "say")
        enode (str "sibyl@" (.getHostName (java.net.InetAddress/getLocalHost)))]

    (log/notice "Started:" (.node    node))
    (log/notice "Mailbox:" (.getName mbox))
    (log/debug  "Cookie :" (.cookie  node))
    (log/debug  "Pinging:" (.ping node enode 1000))

    ;; Start up Say-Sila's hierarchical "mind"
    (inua/create!)

    ;; Receive and process messages from Erlang
    (otp-loop node mbox)
    (.close node)
    :ok)))



;;; --------------------------------------------------------------------------
(defn go!
  "Shortcut to run the experiment current under study.  Expect this function
  to change frequently!"
  []
  (log/notice "Minimum status count:" (cfg/?? :sila :min-statuses))
  (sila/create-world! :env
                      "/srv/say_sila/weka/tweets/tweets.all.env.2019-Q4-2020-Q1Q2.T01.U01.arff"
                      "/srv/say_sila/weka/tweets/tweets.all.env.2019-Q4-2020-Q1Q2.T01.S02.arff")
  (sila/report-world :no-pos)
  (sila/report-concepts))


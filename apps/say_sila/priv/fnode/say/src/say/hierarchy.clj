;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Hierarchical reasoning architecture.
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.hierarchy
  (:require [say.genie  :refer :all]
            [say.config :as cfg]
            [say.log    :as log]
           ;[say.sila   :as sila]
            [weka.core  :as weka]
            [clojure.core.async :as a :refer [>! <! >!! <!! chan go-loop]]
            [clojure.string     :as str])
  (:import  [weka.filters.unsupervised.attribute TweetToInputLexiconFeatureVector   ; emo/bws
                                                 TweetToGenderFeatures]))

(set! *warn-on-reflection* true)

(def ^:const BUFFER     16)
(def ^:const CONN-KEYS  [:ml-sig :ml-gnd-sig :ml-gnd-fbk :dl-gnd-sig])

;; A Layer record defines connections for levels (ML, DL, and eventually PL)
;; as well as the layers (gender, politcal party, etc...) inside those levels.
;;            +---+
;;   sig-in-->| L |-->sig-out
;;            | a |
;;            | y |
;;  fbk-out<--| e |<--fbk-in
;;            | r |
;;            +---+
(defrecord Layer [sig-in sig-out
                  fbk-in fbk-rout])

(defonce Inua (atom nil))                   ; Possessor/master/spirit/soul [Inuit]
(defonce Tell (atom nil))


;;; --------------------------------------------------------------------------
(defn connect
  "Creates a channel for inter level/layer communications."
  ([]
  (chan BUFFER))

  ([chan-keys]
  (reduce #(assoc %1 %2 (connect)) {} chan-keys)))



;;; --------------------------------------------------------------------------
(defn- shutdown
  "Logs a shutdown notification.  If the caller specifies an channel, we
  forward the quit command."
  ([tag]
  (log/notice "Shutting down" (KEYSTR tag) "layer"))

  ([tag out]
  (shutdown tag)
  (>!! out :quit)))



;;; --------------------------------------------------------------------------
(defn shutdown!
  "Shuts down the running hierarchy."
  []
  (doseq [c Inua]
    (>!! c :quit))

  (doseq [a [Inua Tell]]
    (reset! a nil)))



;;; --------------------------------------------------------------------------
(defn- go-DL
  "Starts up a description-logic layer.  This layer currently has no output.
  The application may see its work reflected in the say-sila ontology."
  [sig-in fbk-out]
  ;; Processing loop for the description logic layer.
  (go-loop [msg (<!! sig-in)]
    (if (= :quit msg)
        (shutdown :dl)
        (do (log/info "DL:" msg)
            (recur (<!! sig-in)))))

    ;; This is the final layer
    (->Layer sig-in nil nil fbk-out))



;;; --------------------------------------------------------------------------
(defn- go-ML-gender
  "Starts up the gender module in the machine-learning level."
  [conns ecfg]
  (let [{signal   :ml-gnd-sig
         feedback :ml-gnd-fbk} conns
        genderer  (comment (weka/prep-emoter (TweetToGenderFeatures.) ecfg))] ; TODO

    ;; We receive Weka instances on our signal channel
    (go-loop [insts (<!! signal)]
      (when-not (= :quit insts)
        :todo))))



;;; --------------------------------------------------------------------------
(defn- go-ML
  "Starts up a machine-learning level and returns its output channel."
  [conns]
  (let [ecfg   (cfg/? :emote)
        signal (:ml-sig conns)
        emoter (weka/prep-emoter (TweetToInputLexiconFeatureVector.) ecfg)]

    ;; Start up component layers
    (go-ML-gender conns ecfg)

    ;; Processing loop for the machine-learning layer.
    (go-loop [arff (<!! signal)]
      (when-not (= :quit arff)
          (let [einsts (doto (weka/load-arff arff)
                             (weka/filter-instances emoter []))]
              (log/info "ML:" arff)
              (doseq [c [:ml-gnd-sig]]  ; TODO: forward emos to other layers...
                (>! c einsts))
              (recur (<!! signal)))))))



;;; --------------------------------------------------------------------------
(defn create!
  "Creates and initializes the hierarchical reasoning architecture."
  []
  ;; Set up the hierarchy...
  (swap! Inua #(if % % (let [conns (connect CONN-KEYS)]
                         (log/notice "Creating the hierarchy of mind")
                         (go-ML conns)
                         (go-DL conns)
                         conns)))

  ;; Return the main input channel (also exported as a convenience)
  (reset! Tell (:ml-sig @Inua)))


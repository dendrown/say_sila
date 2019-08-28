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

(def ^:const BUFFER 16)

(defrecord Level [sig-in sig-out
                  fbk-in fbk-rout])

(defonce Mind (atom nil))
(defonce Tell (chan BUFFER))


;;; --------------------------------------------------------------------------
(defn connect
  "Creates a channel for inter level/layer communications."
  []
  (chan BUFFER))



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
  (>!! Tell :quit)
  (reset! Mind nil))



;;; --------------------------------------------------------------------------
(defn- go-dl
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
    (->Level sig-in nil nil fbk-out))



;;; --------------------------------------------------------------------------
(defn- go-ml
  "Starts up a machine-learning layer and returns its output channel."
  [sig-in]
  (let [sig-out (connect)
        fbk-in  (connect)
        emoter  (weka/prep-emoter (TweetToInputLexiconFeatureVector.)
                                  (cfg/? :emote))]

    ;; Processing loop for the machine-learning layer.
    (go-loop [arff (<!! sig-in)]
      (if (= :quit arff)
          (shutdown :ml sig-out)
          (let [einsts (doto (weka/load-arff arff)
                             (weka/filter-instances emoter []))]
              (log/info "ML:" arff)
              (>! sig-out einsts)
              (recur (<!! sig-in)))))

    ;; This is the first layer, so we won't generate feedback.
    ;; Our output channel will be the input for the next layer
    (->Level sig-in sig-out fbk-in nil)))



;;; --------------------------------------------------------------------------
(defn create!
  "Creates and initializes the hierarchical reasoning architecture."
  []
  (swap! Mind #(if % % (let [ml (go-ml Tell)
                             dl (go-dl (:sig-out ml)
                                       (:fbk-in  ml))]
                         (log/notice "Created the hierarchy of mind")
                         {:ml ml
                          :dl dl})))

  ;; Return the main input channel (also exported as a convenience)
  Tell)

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
           ;[weka.core  :as weka]
            [clojure.core.async :as a :refer [>! <! >!! <!! chan go-loop]]
            [clojure.string     :as str]))

(set! *warn-on-reflection* true)

(def ^:const BUFFER 16)

(defonce Mind (atom nil))
(defonce Tell (chan BUFFER))


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
(defn go-dl
  "Starts up a description-logic layer.  This layer currently has no output.
  The application may see its work reflected in the say-sila ontology."
  [in]
  (let [out :say-sila]

    ;; Processing loop for the description logic layer.
    (go-loop [msg (<!! in)]
      (if (= :quit msg)
          (shutdown :dl)
          (do (log/info "DL:" msg)
              (recur (<!! in)))))

    ;; Ontology keyword stands in place of an output channel.
    out))



;;; --------------------------------------------------------------------------
(defn go-ml
  "Starts up a machine-learning layer and returns its output channel."
  [in]
  (let [out (chan BUFFER)]

    ;; Processing loop for the machine-learning layer.
    (go-loop [msg (<!! in)]
      (if (= :quit msg)
          (shutdown :ml out)
          (do (log/info "ML:" msg)
              (>! out msg)
              (recur (<!! in)))))

    ;; Our output channel will be the input for the next layer
    out))



;;; --------------------------------------------------------------------------
(defn create
  "Creates and initializes the hierarchical reasoning architecture."
  []
  (swap! Mind #(if % % (let [ml-> (go-ml Tell)
                             dl-> (go-dl ml->)]
                         (log/notice "Created the hierarchy of mind")
                         {:ml-out ml->
                          :dl-out dl->})))

  ;; Return the main input channel (also exported as a convenience)
  Tell)

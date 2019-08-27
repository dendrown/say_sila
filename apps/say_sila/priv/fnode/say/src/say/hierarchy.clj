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
  (:require [say.config :as cfg]
            [say.log    :as log]
           ;[say.sila   :as sila]
           ;[weka.core  :as weka]
            [clojure.core.async :as a :refer [>! <! >!! <!! go chan thread]]
            [clojure.string     :as str]))

(set! *warn-on-reflection* true)

(defonce TELL (chan 16))


;;; --------------------------------------------------------------------------
(defn create
  "
  Creates and initializes the hierarchical reasoning architecture.
  "
  []
  (thread
    (loop [msg (<!! TELL)]
      (when-not (= :quit msg)
        (log/info "MSG:" msg)
        (recur (<!! TELL))))))


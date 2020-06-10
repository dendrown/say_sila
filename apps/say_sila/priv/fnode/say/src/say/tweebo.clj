;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Say-Sila interface to TweeboParser
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.tweebo
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [clojure.java.io    :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint     :refer [pp]]
            [clojure.string     :as str]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const TWEEBO-EXEC    "/usr/local/bin/tweebo")

(defonce Runner     (agent 0))
(defonce Tweebo-Dir (str (System/getProperty "user.dir") "/resources/tweebo/"))


;;; --------------------------------------------------------------------------
(defn- go-prepare
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [runs tid text]
  (let [ipath (str Tweebo-Dir tid)
        opath (str ipath ".predict")]
    (if (.exists (io/file opath))
      runs
      (do
        (log/debug "Parsing dependencies:" ipath)
        (spit ipath text)
        (let [{:keys [err
                      exit
                      out]} (sh/sh TWEEBO-EXEC ipath)]
          (if (zero? exit)
              ;; TweeboParser seems to be writing normal output to stderr
              (do (log/fmt-debug "Tweebo on ~a: ~a" tid (last (str/split err #"\n")))
                  (inc runs))

              ;; Errors are also going to stderr
              (do (log/fmt-error "Tweebo failure on ~a: ~a: rc[~a]" tid err exit)))
                  runs)))))



;;; --------------------------------------------------------------------------
(defn prepare
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [tid text]
  (send-off Runner go-prepare tid text))


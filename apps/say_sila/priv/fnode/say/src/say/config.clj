;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Configuration utilities for Say Sila
;;;;
;;;; @copyright 2018-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.config
  (:require [say.log         :as log]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [clojure.string  :as str]))

(set! *warn-on-reflection* true)


(def ^:const CONFIG-FPATHS  {:stub   "config/say."
                             :extn   ".config"
                             :base   "config/say.config"
                             :sample "config/say.config.sample"})

(def CONFIG-FPATH   (atom (:base CONFIG-FPATHS)))
(def RNG-SEED       (atom 1248))


;;; --------------------------------------------------------------------------
(defn ^Long rng-seed
  "
  Handle random number generator
  "
  ([]
  (let [seed @RNG-SEED]
    (swap! RNG-SEED inc)
    seed))

  ([seed]
  (swap! RNG-SEED (fn [_] (long seed)))))



;;; --------------------------------------------------------------------------
(defn get-fpath
  "
  Returns the local filepath for the current configuration.
  "
  []
  @CONFIG-FPATH)


;;; --------------------------------------------------------------------------
(defn get-config
  "
  Returns the running instance's configuration as a map.
  "
  []
  (clojure.edn/read-string (slurp @CONFIG-FPATH)))



;;; --------------------------------------------------------------------------
(defn set!
  "
  Changes the active configuration for the specified market and returns the
  new configuration (local) filepath.
  "
  [key & keys]
  (let [fpval (CONFIG-FPATHS key)
        fpath (if fpval
                  fpval
                  (str (:stub CONFIG-FPATHS)
                       (str/join "." (map name (conj keys key)))
                       (:extn CONFIG-FPATHS)))]

    (log/debug "Config:" fpath)
    (when (.exists (io/file fpath))
      (swap! CONFIG-FPATH (fn [_] fpath)))))



;;; --------------------------------------------------------------------------
(defn set-config!
  "
  Changes the active configuration for the specified market and returns the
  map representing the new configuration.
  "
  [key & keys]
  (apply set! key keys)
  (get-config))



;;; --------------------------------------------------------------------------
(defn ?
  "
  Returns the configured value for the specified parameter(s).  If the caller
  includes a second parameter, this value is used as a default if the key
  does not exist in the configuration.
  "
  ([param]
  (? (get-config) param nil))


  ([konf x]
  (if (map? konf)
      (? konf x nil)                ; konf=sub-map, x=key
      (? (get-config) konf x)))     ; konf=key, x=default


  ([conf param default]
  (let [value (conf param)]
    (if value value default))))



;;; --------------------------------------------------------------------------
(defn ??
  "
  Returns the configured value for a parameter buried inside a sub-map in
  the main configuration map.

  IMPORTANT: callers supplying the configuration map (first of four args)
             *must* specify a default value (fourth of four args).
  "
  ([outer inner]
  (?? outer inner nil))


  ([outer inner default]
  (?? (get-config) outer inner default))


  ([conf outer inner default]
  (let [info  (conf outer)
        value (info inner)]

    (if value value default))))



;;; --------------------------------------------------------------------------
(defn ¿
  "
  Returns the configured value for the specified parameter(s).  A singleton
  parameter returns a singleton value (like #'? ).  A sequence of parameters
  returns a sequence of values.
  "
  [param & params]
  (let [conf (get-config)]
    (if params
        (map conf (conj params param))
        (conf param))))


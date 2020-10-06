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
                             :sample "config/say.config.sample"
                             :senti  "config/senti.config.sample"})

(defonce Config         (atom {}))
(defonce RNG-Seed       (atom 1248))


;;; --------------------------------------------------------------------------
(defn ^Long rng-seed
  "Handle random number generator."
  ([]
  (let [seed @RNG-Seed]
    (swap! RNG-Seed inc)
    seed))

  ([seed]
  (swap! RNG-Seed (fn [_] (long seed)))))



;;; --------------------------------------------------------------------------
(defn get-config
  "Returns the running instance's configuration as a map."
  []
  @Config)



;;; --------------------------------------------------------------------------
(defn set-config!
  "Changes the active configuration for the specified market and returns the
  map representing the new configuration."
  [key & keys]
  (let [fpval (CONFIG-FPATHS key)
        fpath (if fpval
                  fpval
                  (str (:stub CONFIG-FPATHS)
                       (str/join "." (map name (conj keys key)))
                       (:extn CONFIG-FPATHS)))]

    (log/debug "Config:" fpath)
    (if (.exists (io/file fpath))
      (reset! Config (merge (edn/read-string (slurp fpath))
                            {:fpath fpath}))
      (log/warn "Configuration unchanged"))

    @Config))



;;; --------------------------------------------------------------------------
(defn ?
  "Returns the configured value for the specified parameter(s).  If the caller
  includes a second parameter, this value is used as a default if the key
  does not exist in the configuration."
  ([param]
  (? @Config param nil))


  ([konf x]
  (if (map? konf)
      (? konf x nil)                ; konf=sub-map, x=key
      (? @Config konf x)))          ; konf=key, x=default


  ([conf param default]
  (get conf param default)))



;;; --------------------------------------------------------------------------
(defn ??
  "Returns the configured value for a parameter buried inside a sub-map in
  the main configuration map.

  IMPORTANT: callers supplying the configuration map (first of four args)
             *must* specify a default value (fourth of four args)."
  ([outer inner]
  (?? outer inner nil))


  ([outer inner default]
  (?? @Config outer inner default))


  ([conf outer inner default]
  (let [info  (get conf outer)
        value (get info inner)]

    (if value value default))))



;;; --------------------------------------------------------------------------
(defn ¿
  "Returns the configured value for the specified parameter(s).  A singleton
  parameter returns a singleton value (like #'? ).  A sequence of parameters
  returns a sequence of values."
  [param & params]
  (let [conf @Config]
    (if params
        (map conf (conj params param))
        (conf param))))



;;; --------------------------------------------------------------------------
(defn !
  "Updates the configuration, setting the value associated with the specified
  parameter key."
  ([param value]
  (swap! Config assoc param value)))



;;; --------------------------------------------------------------------------
(defn !!
  "Updates the configuration, setting the value associated with the inner key
  inside the submap specified by the outer key (:outer => :inner => value)."
  ([outer inner value]
  (swap! Config assoc-in [outer inner] value)))



;;; --------------------------------------------------------------------------
(set-config! :base)                     ; WARNING: Causes gen-class AOT to hang

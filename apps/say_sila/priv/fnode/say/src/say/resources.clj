;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Application resource handling
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.resources
  (:require [clojure.string     :as str]))


(set! *warn-on-reflection* true)

(defonce Separator  (java.io.File/separator))


;;; --------------------------------------------------------------------------
(defn- attach-project-dir
  "Returns a directory path under the main project directory."
  [& dirs]
  (apply str (interpose Separator
                        (conj dirs (System/getProperty "user.dir")))))



;;; --------------------------------------------------------------------------
(defn- attach-resource-dir
  "Returns a directory path under the project resources directory."
  [& dirs]
  (apply attach-project-dir "resources" dirs))



;;; --------------------------------------------------------------------------
(defn get-dir
  "Determines if dir is referring to a project-relative or an absolute path
  and returns the equivalent absolute path.  We allow a default path for
  when this function is invoked on a configuration setting which may be nil."
  ([dir]
  (get-dir dir nil))


  ([dir dflt]
  (cond
    ;; Do we have something to work with?
    (nil? dir)
        (if (some? dflt)
            (recur dflt nil)                        ; Try the default
            (System/getProperty "java.io.tmpdir"))  ; No love, give 'em /tmp

    ;; Absolute path is returned as is?
    (str/starts-with? dir Separator)
      dir

    ;; Already has the resource relative path?
    (str/starts-with? dir (str "resources" Separator))
      (attach-project-dir dir)

    ;; Any other actual value goes under the resources directory
    :else
      (attach-resource-dir dir))))



;;; --------------------------------------------------------------------------
(defn get-fpath
  "Returns the filepath give the directory stub and the filename."
  [fstub fname]
  (let [sep (if (str/ends-with? fstub Separator)
                ""
                Separator)]
    (str (get-dir fstub) sep fname)))


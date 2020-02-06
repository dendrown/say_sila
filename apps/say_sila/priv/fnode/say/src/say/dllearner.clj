;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Say-Sila interface to DL-Learner
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.dllearner
  (:require [say.genie          :refer :all]
            [clojure.java.io    :as io]
            [clojure.string     :as str]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const DLL-DIR    "resources/dllearner/")

(defonce BASE-CONF      (str DLL-DIR "say-senti.base.conf"))


;;; --------------------------------------------------------------------------
(defn make-config-fpath
  "Creates a DL-Learner configuration filepath from a filename stub."
  [fstub]
  (str (when-not (str/starts-with? fstub DLL-DIR) DLL-DIR)
       fstub
       (when-not (some #(str/ends-with? fstub %) [".conf" ".cnf" ".ini"])
         ".conf")))



;;; --------------------------------------------------------------------------
(defn print-pn-examples
  "Prints the IDs of the tweets with positive polarities and those with
  negative polarities.  The caller may optionally specify a prefix for easy
  insertion into a DL-Learner configuration file."
  ([xmps]
  (print-pn-examples xmps 6))


  ([xmps n]
  (let [delims  (conj (repeat \,) \space)
        liner   #(apply print "\n    " %1 (interpose \, (domap pr-str %2)))]
  ;; Report our P/N examples
  (doseq [[klass xs] xmps]
    (print (str "lp." (name klass) "Examples = {"))
    (domap liner delims (partition-all n xs))
    (println "\n}")))))



;;; --------------------------------------------------------------------------
(defn write-pn-config
  "Creates a DL-Learner configuration file from the specified config map."
  [fname xmps]
  ;; Most of the config is in the base.  We need to add the p/n examples.
  (with-open [wtr (io/writer (make-config-fpath fname))]
    (.write wtr (slurp BASE-CONF))
    (binding [*out* wtr]
      (print-pn-examples xmps))))


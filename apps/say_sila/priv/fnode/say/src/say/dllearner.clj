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
(def ^:const KB-DIR     "resources/KB/")
(def ^:const PREFIXES   {"owl"   "http://www.w3.org/2002/07/owl#"
                         "dul"   "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#"})

(defonce DELIMS         (conj (repeat \,) \space))      ; For printing comma-separated series

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
  (let [liner #(apply print "\n    " %1 (interpose \, (domap pr-str %2)))]

    ;; Report our P/N examples
    (doseq [[klass xs] xmps]
      (print (str "lp." (name klass) "Examples = {"))
      (domap liner DELIMS (partition-all n xs))
      (println "\n}")))))



;;; --------------------------------------------------------------------------
(defn print-prefixes
  "Prints DL-Learner prefixes (probably to a rebout *out*)."
  [prefixes]
  (letfn [(prt-prefix [delim [pre iri]]
            (println (str "    " delim " (\"" pre "\", \"" iri "\")")))]

    ;; Write the prefixes out in DL-Learner config style
    (println "prefixes = [")
    (domap prt-prefix DELIMS (merge PREFIXES prefixes))
    (println "]")))



;;; --------------------------------------------------------------------------
(defn write-pn-config
  "Creates a DL-Learner configuration file from the specified config map."
  [& {:keys [base rule prefixes examples]}]
  ;; Most of the config is in the base.  We need to add the p/n examples.
  (let [fname       (str base "-" (name rule))
        fpath       (make-config-fpath fname)
        [head body] (map #(slurp (str DLL-DIR base "." (name %) ".conf"))
                         [:head :body])]
    (with-open [wtr (io/writer fpath)]
      (binding [*out* wtr]
        (println head)
        (println (str "ks.fileName = \"KB/" fname ".owl\""))
        (print-prefixes prefixes)
        (println body)
        (print-pn-examples examples)))
  fpath))



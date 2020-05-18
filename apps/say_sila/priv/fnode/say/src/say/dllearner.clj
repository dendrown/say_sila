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
            [say.config         :as cfg]
            [say.log            :as log]
            [tawny.owl]
            [tawny.english]
            [clojure.java.io    :as io]
            [clojure.java.shell :as sh]
           ;[clojure.core.match :refer [match]]
            [clojure.pprint     :refer [pp pprint]]
            [clojure.string     :as str]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const DLL-EXEC   "/usr/local/bin/dll")
(def ^:const DLL-DIR    "resources/dllearner/")
(def ^:const FRAME-DIR  "resources/dllearner/frame/")
(def ^:const KB-DIR     "resources/KB/")

(def ^:const INDENT     "    ")
(def ^:const PREFIXES   {"owl"   "http://www.w3.org/2002/07/owl#"
                         "dul"   "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#"})

(def ^:const INIT-ALGORITHM :ocel)
(def ^:const INIT-REASONER  :cwa)


(defonce Delimiters     (conj (repeat \,) \space))              ; For printing comma-separated series
(defonce Symbols        (atom (apply merge                      ; Function and class variable lookup
                                     {'min #'tawny.owl/at-least
                                      'max #'tawny.owl/at-most}
                                     (map ns-publics '[tawny.owl tawny.english]))))


;;; --------------------------------------------------------------------------
;;; FIXME: We're going to have a more natural "solution" if we pass around the
;;;        basic s-expression and attach the other fields as metadata.
(defrecord Solution
  [rule acc f1 length depth])


;;; --------------------------------------------------------------------------
(defn register-ns
  "Add the symbols from the caller's namespace to the DLL-OWL Symbols map."
  ([]
  (register-ns *ns*))                                   ; Caller's namespace


  ([nspace]
  (swap! Symbols merge (ns-publics nspace))))



;;; --------------------------------------------------------------------------
(defprotocol Converter
  (->tawny [s] "Reorders a solution sequence such that it may be used with Tawny-OWL."))

(extend-protocol Converter

  clojure.lang.ASeq
  (->tawny [s]
    ;(log/debug (log/<> "TAWNY" (count s)) s)
    (let [reorder (fn [ord]
                    (apply list (map #(->tawny (nth s %)) ord)))]
    (case (count s)
       1 (->tawny (first s))                        ; promote (Class)
       2 (map ->tawny s)                            ; (not (...))
       3 (reorder [1 0 2])                          ; infix -> prefix
       4 (reorder [1 2 0 3]))))                     ; (oprop min 5 (..))

  Object
  (->tawny [obj]
    ;; Lookup the namespace-qualified symbol
    (if-let [sym (get @Symbols obj)]
      sym
      obj)))



;;; --------------------------------------------------------------------------
(defn read-solution
  "Converts a string representing a DLL-Learner solution into a Solution
  object with a rule that can be handled by Tawny-OWL.  If the input text
  is a comment (starts with a semicolon), the function returns nil."
  [text]
  ;; Check for comments explicitly as we'll be adding parens before calling Lisp
  (when (not= \; (first (str/triml text)))
    ;; Tame and lispify the DL-Learner output string
    (let [soln (read-string (str \(
                                 (reduce (fn [txt [re k]]
                                           (str/replace txt re k))
                                          text
                                          [[#"accuracy"    ":acc"]    ; OCEL  stats
                                           [#"pred. acc.:" ":acc"]    ; CELOE stats
                                           [#"F-measure:"  ":f1"]
                                           [#"%"           ""]])
                                 \) ))
          [rule
           stats] (butlast-last soln)]

      ;; Convert the DLL rule into a record
      (map->Solution (apply hash-map :rule (->tawny rule) stats)))))



;;; --------------------------------------------------------------------------
(defn make-config-fpath
  "Creates a DL-Learner configuration filepath from a filename stub."
  ([fstub]
  (str (when-not (str/starts-with? fstub DLL-DIR) DLL-DIR)
       (name fstub)
       (when-not (some #(str/ends-with? fstub %) [".conf" ".cnf" ".ini"])
         ".conf")))


  ([fstub & fstubs]
  ;; We forced a parameter, but now we need to reconstruct the full list of stubs
  (make-config-fpath (apply str (interpose "-" (map name (conj fstubs fstub)))))))


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
      (domap liner Delimiters (partition-all n xs))
      (println "\n}")))))



;;; --------------------------------------------------------------------------
(defn indent
  "Creates a string beginning with INDENT spaces followed by the passed values."
  [& ss]
  (apply str (interpose " " (conj ss INDENT))))



;;; --------------------------------------------------------------------------
(defn println-indent
  "Prints INDENT spaces followed by the passed values."
  [delim & ss]
  (print (indent delim))
  (domap pr ss)
  (prn))



;;; --------------------------------------------------------------------------
(defprotocol ConfigPrinter
  (prn-config-value [v] "Formats a value for a DL-Learner configuration based on its type"))


(extend-protocol ConfigPrinter

  clojure.lang.Keyword
  (prn-config-value [v]
    (prn (name v)))


  clojure.lang.Symbol
  (prn-config-value [v]
    (println \{ v \}))


  clojure.lang.PersistentVector
  (prn-config-value [vv]
    (println \{)
    (doseq [[d v] (zip Delimiters vv)]
      (println-indent d v))
    (println \}))


  Object
  (prn-config-value [v]
    (prn v)))



;;; --------------------------------------------------------------------------
(defn print-prefixes
  "Prints DL-Learner prefixes (probably to a rebound *out*)."
  [prefixes]
  (letfn [(prt-prefix [delim [pre iri]]
            (println (str "    " delim " (\"" pre "\", \"" iri "\")")))]

    ;; Write the prefixes out in DL-Learner config style
    (println "prefixes = [")
    (domap prt-prefix Delimiters (merge PREFIXES prefixes))
    (println "]")))



;;; --------------------------------------------------------------------------
(defn- print-config-submap
  "Outputs a configuration section, corresponding to a DL-Learner object,
  using values from the submap in conf identified by ctag."
  ([section conf]
  ;; In the simplest case, the section *is* the ctag lookup key
  (print-config-submap section conf section))


  ([section conf ctag]
  (let [obj (name section)]
    ;; Add the values under Say-Sila's configuration
    (doseq [[k v] (conf ctag)]
      (print (str obj "." (name k)) "= ")
      (prn-config-value v))))


  ([section conf ctag heading & {:as lines}]
  ;; First handle config lines that never change
  (println "\n//" (name heading))
  (when lines
    (print-config-submap section {:doit lines} :doit))

  ;; Add on the elements from the configuration
  (print-config-submap section conf ctag)))



;;; --------------------------------------------------------------------------
(defn print-config-reasoner
  "Handles the 'reasoner' section of the DL-Learner configuration."
  ([]
  (print-config-reasoner (cfg/? :dllearner)))


  ([conf]
  (print-config-submap :reasoner                        ; DL-Learner config section/object
                       conf                             ; Say-Sila's :dllearner submap
                       (:reasoner conf INIT-REASONER)   ; Open/Closed world assumption
                       "Reasoner"
                       :sources 'ks)))



;;; --------------------------------------------------------------------------
(defn print-config-algorithm
  "Handles the 'Learner Algorithm' section of the DL-Learner configuration."
  ([]
  (print-config-algorithm (cfg/? :dllearner)))


  ([conf]
  (let [calg (get-in conf [:alg :type] INIT-ALGORITHM)]         ; Configured ocel|celoe
    (print-config-submap :alg conf :alg "Learning Algorithm")
    (print-config-submap :alg conf calg))))



;;; --------------------------------------------------------------------------
(defn write-pn-config
  "Creates a DL-Learner configuration file from the specified config map."
  [& {:keys [base rule prefixes examples]}]
  ;; DLL configuration details are in our app's config.
  ;; After that, we need to add the p/n examples.
  (let [conf    (cfg/? :dllearner)
        fname   (str base "-" (name rule))
        fpath   (make-config-fpath fname)
        head    (slurp (str FRAME-DIR base ".head.conf"))]

    (log/info "Creating DL-Learner configuration:" fpath)
    (with-open [wtr (io/writer fpath)]
      (binding [*out* wtr]
        (println head)
        (println (str "ks.fileName = \"KB/" fname ".owl\""))
        (print-prefixes prefixes)
        (doseq [f [print-config-reasoner
                   print-config-algorithm]]
          (f conf))
        (prn)
        (print-config-submap :lp conf)
        (print-pn-examples examples)))
    fpath))



;;; --------------------------------------------------------------------------
(defn find-solutions
  "Processes DL-Learner output and returns a sequence of Solutions."
  [{:keys [err exit out]}]
  (letfn [(collect [solns line]
            ;; We're reducing a block of lines of the form "N: (...)"
            (if-let [soln (-> line
                              (str/split #": " 2)
                              second)]
              (conj solns soln)
              (reduced solns)))]

    ;; Was the run successful?
    (if (empty? err)
      ;; Process the DL-Learner output
      (loop [lines (str/split-lines out)]
        ;; Run through the lines until we get to the solution block
        (when-let [line (first lines)]
          (log/debug line)
          (if (and (str/starts-with? line "solutions")          ; Next line starts them?
                   (str/ends-with? line ":"))
              (reduce collect [] (rest lines))                  ; - Return solutions
              (recur (rest lines)))))                           ; - Keep chugging through output
      ;; Report the badness
      (log/fmt-error "~a: rc[~a]" err exit))))



;;; --------------------------------------------------------------------------
(defn run
  "Runs a DL-Learner session to determine equivalent classes for Positive Texts."
  [& tags]
  (let [dconf (apply make-config-fpath tags)
        exec  (cfg/?? :dllearner :exec DLL-EXEC)]
    (log/fmt-notice "Running DL-Learner: cfg[~a]" dconf)
    (find-solutions (sh/sh exec "-c" dconf))))


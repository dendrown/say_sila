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
            [clojure.data.csv   :as csv]
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
(defn get-fpath
  "Returns the filepath associated with a endency tree for later use."
  ([tid]
  (get-fpath tid nil))

  ([tid kind]
  (apply str Tweebo-Dir tid (when kind
                              ["." (name kind)]))))



;;; --------------------------------------------------------------------------
(defn- go-prepare
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [runs tid text]
  (let [ipath (get-fpath tid)
        opath (get-fpath tid :predict)]
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



;;; --------------------------------------------------------------------------
(defn predict
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [tid]
  (with-open [rdr (io/reader (get-fpath tid :predict))]
    (doall (csv/read-csv rdr :separator \tab))))



;;; --------------------------------------------------------------------------
(defn wait
  "Blocks execution until all pernding Tweebo Parser requests have completed."
  []
  (log/fmt-info "Syncing Tweebo requests: cnt[~a]" @Runner)
  (await Runner))



;;; --------------------------------------------------------------------------
(defn print-tree
  "Prints a tree structure to show dependencies."
  [{:keys [tid content]}]

  (letfn [(strip [[ndx tok _ _ _ _ dep]]                        ; Lose the uneeded elements
            [ndx tok dep])

          (child? [[_ _ dep] pix]                               ; Is the dependency the parent index (pix)?
            (= dep pix))

          (omit? [i]
            (or (nil? i)
                (child? i "-1")))

          (root? [i]
            (child? i "0"))

          (proc [prefix [pix parent _] children]
            (println prefix parent)
            (run! #(if (child? % pix)
                        (proc (str "  " prefix) % children))
                  children))

            ]
    ;; Process the Tweebo parser output
    (let [parsed (map strip (predict tid))
          {roots    true
           children false} (group-by root? (remove omit? parsed))]

      (loop [roots roots]
        (when-let [root (first roots)]
          (proc "+" root children)
          (println)
          (recur (rest roots)))))))

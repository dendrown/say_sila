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
  [{:keys [etokens pos-tags tid]
    :or   {etokens (repeat nil)}}]

   (letfn [; -----------------------------------------------------------------
           (combine [[etok pos [ndx tok _ _ _ _ dep]]]
             ;; Take just the elements we need
             [ndx dep pos (if etok
                              etok
                              tok)])

           ; -----------------------------------------------------------------
           (child? [[_ dep _ _] pix]
             ;; Is the dependency the parent index (pix)?
             (= dep pix))

           ; -----------------------------------------------------------------
           (omit? [i]
             (or (nil? i)
                 (child? i "-1")))

           ; -----------------------------------------------------------------
           (root? [i]
             (child? i "0"))

           ; -----------------------------------------------------------------
           (proc [lvl finals? [pix _ pos parent] children]
             ;; With the short token lists, we always check all the children
             (let [[fins?-a
                    fin?-z] (butlast-last finals?)
                   branch   (if fin?-z "└" "├")
                   indent   (if (zero? lvl)
                                ""
                                (apply str (map #(if % "    "
                                                       "│   ") fins?-a)))
                   kids     (filter #(child? % pix) children)]
               ;; Print the current node and recursively process its children
               (log/fmt! "~a~a── ~a (~a)\n" indent branch parent pos)
               (process (inc lvl) finals? kids children)))

           ; -----------------------------------------------------------------
           (process [lvl finals? parents-az children]
             (let [[parents-a
                    parent-z] (butlast-last parents-az)]
             ;; If there's only one parent, it'll be the last parent
             (when parent-z
               (run! #(proc lvl (conj finals? false) % children) parents-a)
               (proc lvl (conj finals? true) parent-z children))))]

    ;; Process the Tweebo parser output
    (let [parsed (map combine (zip etokens
                                   pos-tags
                                   (predict tid)))
          {roots    true
           children false} (group-by root? (remove omit? parsed))]

      (println "•")
      (process 0 [] roots children)
      (println))))


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
  [say.resources]
            [say.config         :as cfg]
            [say.label          :as lbl]
            [say.log            :as log]
            [say.resources      :as rsc]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint     :refer [pp]]
            [clojure.string     :as str]
            [me.raynes.fs       :as fs]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Subdir-Tweet-Cut   3)                      ; (Final) digits from tweet ID
(def ^:const Subdir-User-Cut    2)                      ; Letters from user ID
(def ^:const Subdir-Unknown-Cut 4)                      ; Characters from unrecognized ID
(def ^:const Tweebo-Exec    "/usr/local/bin/tweebo")

(defonce Runner     (agent 0))
(defonce Tweebo-Dir (rsc/get-dir (cfg/?? :tweebo :dir) "tweebo"))


;;; --------------------------------------------------------------------------
(defn get-subdir
  "Returns the  subdirectory (under the tweebo resource directory) where a
  user's tweet or profile analysis should go."
  [fname]
  ;; The text ID is the simple filename; remove any extension.
  (let [id  (first (str/split fname #"\." 2))
        cnt (count id)]
    (cond
      ;; Use the last digits of a tweet ID. (The initial digits don't vary enough.)
      (str/starts-with? id lbl/Tweet-Tag)
        (subs id (- cnt Subdir-Tweet-Cut))

      ;; Use the first part of the account name for user profiles
      (str/starts-with? id lbl/Profile-Tag)
        (let [skip  (count lbl/Profile-Tag)
              cut   (min cnt
                         (+ skip Subdir-User-Cut))]
          (str/upper-case (subs id skip cut)))

      :else
        (str "_" (subs id 0 Subdir-Unknown-Cut)))))



;;; --------------------------------------------------------------------------
(defn get-fpath
  "Returns the filepath associated with a endency tree for later use."
  ([tid]
  (get-fpath tid nil))

  ([tid kind]
  (rsc/get-fpath Tweebo-Dir
                 (get-subdir tid)
                 (if kind
                     (str tid "." (name kind))
                     tid))))



;;; --------------------------------------------------------------------------
(defn- go-prepare
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [runs tid text]
  (let [ipath (get-fpath tid)
        opath (get-fpath tid :predict)
        ofile (io/file opath)]
    (if (.exists ofile)
      (do ;(log/debug "Tweebo parse exists:" opath)
          runs)
      (do
        (log/fmt-debug "Parsing dependencies: cnt[~a] fp[~a]" runs ipath)
        (.mkdirs (.getParentFile ofile))
        (spit ipath text)
        (let [{:keys [err
                      exit
                      out]} (sh/sh Tweebo-Exec ipath)]
          (if (zero? exit)
              ;; TweeboParser seems to be writing normal output to stderr
              (do (log/fmt-debug "Tweebo on ~a: ~a" tid (last (str/split err #"\n")))
                  (inc runs))

              ;; Errors are also going to stderr
              (do (log/fmt-error "Tweebo failure on ~a: ~a: rc[~a]" tid err exit)
                  runs)))))))



;;; --------------------------------------------------------------------------
(defn prepare
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [tid text]
  (send-off Runner go-prepare tid text))



;;; --------------------------------------------------------------------------
(defn predict
  "Prepares a TweeboParser (predicted) dependency tree for later use."
  [tid]
  (when-let [lines (try
                    (with-open [rdr (io/reader (get-fpath tid :predict))]
                      (doall (csv/read-csv rdr :separator \tab)))
                    (catch Exception ex
                      ;(log/error "Cannot read predicted dependencies:" tid)
                      nil))]
    ;; There's a pesky empty line at the end of the Tweebo output
    (remove #(= % [""]) lines)))


;;; --------------------------------------------------------------------------
(defn get-terms
  "Returns a lazy sequence of tbe terms, as parsed by Tweebo, for a given
  tweet ID.  If the tweet has not previously been parsed by Tweebo, the
  function returns an empty list."
  [tid]
  ;; Filter out the nil from the final empty line in the the tweebo output
  (map second (predict tid)))


;;; --------------------------------------------------------------------------
(defn get-pos
  "Returns a lazy sequence of part of speech tags, as parsed by Tweebo, for
  a given tweet ID.  If the tweet has not previously been parsed by Tweebo,
  the function returns an empty list."
  [tid]
  ;; Filter out the nil from the final empty line in the the tweebo output
  (map #(nth % 3) (predict tid)))


;;; --------------------------------------------------------------------------
(defn get-pos-terms
  "For a given tweet ID, returns a lazy sequence of vectors, each containing
  the part of speech tag of a term and the term itself.  If the tweet has not
  previously been parsed by Tweebo, the function returns an empty list."
  [tid & opts]
  ;; Filter out the nil from the final empty line in the the tweebo output
  (when-let [parse (predict tid)]
    (let [pairs (map (fn [[_ term _ pos]]
                       [pos term])
                     parse)]
      (if (some #{:side-by-side} opts)
          (reduce (fn [[poss terms] [p t]]
                    [(conj poss p) (conj terms t)])
                  [[] []]
                  pairs)
          pairs))))


;;; --------------------------------------------------------------------------
(defn wait
  "Blocks execution until all pernding Tweebo Parser requests have completed."
  []
  (log/fmt-debug "Syncing Tweebo requests: cnt[~a]" @Runner)
  (when-not (await-for 30000 Runner)
    (recur)))



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



;;; --------------------------------------------------------------------------
(defn migrate!
  "Development function that copies Tweebo files from the legacy directory
  structure to the new one as defined in the configuration."
  []
  (let [old-dir   (rsc/get-dir "tweebo")
        old-fpath #(str old-dir "/" %)
        ignore    #{".keep" "working_dir" "requote" "requote.l"}]

    (println "Migrating Tweebo files:")
    (println "* SRC:" old-dir)
    (println "* DST:" Tweebo-Dir)
    (println "Please press <ENTER>")
    (read-line)

    (doseq [o (.list (io/file old-dir))]
      (when-not (ignore o)
        (log/debug "Copying" o)
        (fs/copy+ (old-fpath o)
                  (get-fpath o))))))


;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Wrappers fordNet functionality
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.wordnet
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [wordnet.core       :as wnet]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

;;; Boost survey vocabulary with WordNet, if available
(defonce Dictionary (when-let [dir (cfg/?? :sila :wordnet-dir)]
                      (wnet/make-dictionary dir)))


;;; --------------------------------------------------------------------------
(defmacro wordnet
  "If the application has access to WordNet, this macro executes the body
  expression.  If not, it logs a warning and returns its argument unchanged."
  [arg & body]
  `(if Dictionary
      (do ~@body)
      (do (log/warn "WordNet is not configured.")
          ~arg)))



;;; --------------------------------------------------------------------------
(defn synonyms
  "Returns synonyms (lemmas) of the word.  Caution: the synonyms span all
  associated synsets."
  [word]
  (wordnet word
    (->> (Dictionary word)
         (map wnet/synset)
         (mapcat wnet/words)
         (map :lemma)
         (into #{}))))

(def synonyms# (memoize synonyms))



;;; --------------------------------------------------------------------------
(defn synonym-values
  "Finds the synonyms for all values in a hashmap."
  [wmap]
  (wordnet wmap
    (update-values wmap #(into #{} (mapcat synonyms# %)))))


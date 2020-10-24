;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Wrappers WordNet functionality
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.wordnet
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [wordnet.core       :as wnet]
            [clojure.pprint     :refer [pp]]))

(defonce Concept-Synsets
 ;; These are synsets applicable to climate change concepts as described in the Six Americas
 {"save"
  #{"SID-02556565-V"    ;gloss "save from ruin, destruction, or harm"
    "SID-02557529-V"    ;gloss "bring into safety; \"We pulled through most of the victims of the bomb attack\""
    "SID-02470006-V"}}) ;gloss "refrain from harming"


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
(defn synsets
  "Returns synsets for the specified word.  If the word represents a Say-Sila
  concept, the caller will only get the synsets wich are applicable climate
  change modelling.  Otherwise, the synonyms span all associated synsets."
  [word]
  (wordnet word
  (let [syns (get Concept-Synsets word)
        use? #(or (nil? syns)
                  (contains? syns (:id %)))]
    (->> (Dictionary word)
         (map wnet/synset)
         (filter use?)))))



;;; --------------------------------------------------------------------------
(defn synonyms
  "Returns synonyms (lemmas) of the word.  Note that for words representing
  Say-Sila concepts, the function will only return synonyms associated with
  climate change modelling."
  [word]
  (when-let [syns (synsets word)]
    (->> syns
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


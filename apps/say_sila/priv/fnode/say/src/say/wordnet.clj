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
            [clojure.set        :as set]
            [clojure.pprint     :refer [pp]]))

(defonce Concept-Synsets
 ;; These are synsets applicable to climate change concepts as described in the Six Americas
 {"cause"
  #{"SID-07341157-N"    ;gloss "events that provide the generative force that is the origin of something..."
    "SID-00007347-N"    ;gloss "any entity that produces an effect or is responsible for events or results"
    "SID-01649143-V"    ;gloss "give rise to; cause to happen or occur, not always intentionally..."
    "SID-00772482-V"}   ;gloss "cause to do; cause to act in a specified manner"

  "human"
  #{"SID-02474924-N"    ;gloss "any living or extinct member of the family Hominidae characterized by
                        ;       superior intelligence, articulate speech, and erect carriage"
    "SID-02754015-A"    ;gloss "characteristic of humanity; 'human nature'"
    "SID-02754145-A"    ;gloss "relating to a person; 'the experiment was conducted on 6 monkeys and 2 human subjects'"
    "SID-01261689-A"}   ;gloss "having human form or attributes as opposed to those of animals or divine beings..."

  "nature"
  #{"SID-09389659-N"}   ;gloss "the natural physical world including plants and animals and landscapes etc."

 ;; --------------------------------------------------------------------------
  "conservagtion"
  #{"SID-07434199-N"    ;gloss "an occurrence of improvement by virtue of preventing loss or injury or other change"
    "SID-00820935-N"}   ;gloss "the preservation and careful management of the environment and of natural resources"

  "save"
  #{"SID-02556565-V"    ;gloss "save from ruin, destruction, or harm"
    "SID-02557529-V"    ;gloss "bring into safety; \"We pulled through most of the victims of the bomb attack\""
    "SID-02470006-V"}   ;gloss "refrain from harming"

 ;; --------------------------------------------------------------------------
  "cut"
  #{"SID-00430013-V"    ;gloss "cut down on; make a reduction in; 'reduce your daily fat intake'"
    "SID-00244786-V"}   ;gloss "reduce in scope while retaining essential elements; 'The manuscript must be shortened'"

 ;; --------------------------------------------------------------------------
  "protect"
  #{"SID-01130619-V"}   ;gloss "shield from danger, injury, destruction, or damage"

  "environment"
  #{"SID-13957629-N"}   ;gloss "the totality of surrounding conditions"

 ;; --------------------------------------------------------------------------
  "economic"
  #{"SID-02727475-A"    ;gloss "of or relating to an economy, the system of production and management of material wealth"
    "SID-02587892-A"}   ;gloss "concerned with worldly necessities of life (especially money)"

  "growth"
  #{"SID-13518338-N"}   ;gloss "a process of becoming larger or longer or more numerous or more important"
})

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
  change modelling.  Otherwise, the synonyms span all associated synsets.

  When the caller specified multiple words, the function returns the
  intersection of the synsets for these words."
  ([word]
  (wordnet word
  (let [syns (get Concept-Synsets word)
        use? #(or (nil? syns)
                  (contains? syns (:id %)))]
    (->> (Dictionary word)
         (map wnet/synset)
         (filter use?)))))


  ([word & more]
  (apply set/intersection (map #(into #{} (synsets %))
                          (conj more word)))))



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


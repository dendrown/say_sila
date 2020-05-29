;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; CMU Part-of-speech Ontology
;;;;
;;;; @copyright 2019-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.cmu-pos
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.config         :as cfg]
            [say.dllearner      :as dll]
            [say.dolce          :as dul]
            [say.log            :as log]
            [say.social         :as soc]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp pprint]]
            [tawny.reasoner     :as rsn]
            [tawny.repl         :as repl]                       ; <= DEBUG
            [tawny.owl          :refer :all]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/cmu-pos.owl#")
(def ^:const ONT-FPATH  "resources/KB/cmu-pos.owl")


;;; --------------------------------------------------------------------------
(defontology cmu-pos
  :iri    ONT-IRI
  :prefix "pos")
(dul/access)

(defclass Token
  :super   dul/InformationObject
  :label   "Token"
  :comment (str "An Information Object consisting of a single linguistic element of meaning, "
                "generally in textual or verbal form.  A Token very often corresponds to a word, "
                "but in may also represent punctuation, an emoji, or any other output element from "
                "the CMU POS tagger."))

(defclass PartOfSpeech
  :super   dul/Quality
  :label   "Part of Speech"
  :comment (str "A quality describing an appropriate Information Object's part-of-speech"
                " according to the CMU POS tagging system."))

(defoproperty isPartOfSpeech
  :super    dul/hasQuality
  :label    "is part of speech"
  :domain   Token
  :range    PartOfSpeech)

(defdproperty hasPartOfSpeechTag
  :domain  PartOfSpeech
  :range   :XSD_STRING
  :label   "has part of speech tag"
  :comment "Defines a descriptive tag used to designate the part of speech of an Information Object."
  :characteristic :functional)


;;; --------------------------------------------------------------------------
;; CMU POS tags are described in \cite{gimpel2011}
(defmacro defpos
  "Adds a PartOfSpeech subclass to the cmu-pos ontology"
  [pos tag descr]
  `(do (defclass ~pos
         :super   PartOfSpeech
         :label   (str/join " " (soc/tokenize (name '~pos)))
         :comment (str "A Part-of-Speech representing " ~descr)
         :equivalent (has-value hasPartOfSpeechTag ~tag))

       (defpun ~pos)
       (refine (individual (str '~pos))
         :fact (is hasPartOfSpeechTag ~tag))))


;Nominal, Nominal + Verbal
(defpos CommonNoun               "N" "a common noun (NN,NNS)")
(defpos Pronoun                  "O" "a pronoun (personal/WH, not possessive; PRP,WP)")
(defpos NominalPossessive        "S" "a nominal + possessive")
(defpos ProperNoun               "^" "a proper noun (NNP,NNPS)")
(defpos ProperNounPlusPossessive "Z" "a proper noun + possessive")
(defpos NominalVerbal            "L" "a nominal + verbal")
(defpos ProperNounPlusVerbal     "M" "a proper noun + verbal")

; Open-class words
(defpos Verb                     "V" "a verb including copula and auxiliaries (V*,MD)")
(defpos Adjective                "A" "an adjective (J*)")
(defpos Adverb                   "R" "an adverb (R*,WRB)")
(defpos Interjection             "!" "an interjection (UH)")

; Closed-class words
(defpos Determiner               "D" "a determiner (WDT,DT,WP$,PRP$)")
(defpos CoordinatingConjunction  "&" "a coordinating conjunction (CC)")
(defpos VerbParticle             "T" "a verb particle (RP)")
(defpos ExistentialThere         "X" "an existential there, predeterminers (EX,PDT)")
(defpos ExistentialPlusVerbal    "Y" "X+ a verbal")
(defpos Preposition              "P" "a pre- or postposition, or subordinating conjunction (IN,TO)")

; Twitter/social media
(defpos Hashtag                  "#" "a hashtag (indicates a topic/category for a tweet)")
(defpos Address                  "U" "a URL or email address")
(defpos Emoticon                 "E" "an emoticon")
(defpos AtMention                "@" "an at-mention (indicating another use  as a recipient of a tweet)")
(defpos Continuation             "~" "an indicator that the message continues across multiple tweets")

; Miscellaneous
(defpos Numeral                  "$" "a numeral (CD)")
(defpos Punctuation              "," "punctuation")
(defpos Other                    "G" (str "abbreviations, foreign words, possessive endings, "
                                          "symbols, or garbage (FW,POS,SYM,LS)"))

;;; Tell DL-Learner about our ontology elements
(dll/register-ns)


;;; --------------------------------------------------------------------------
(rsn/reasoner-factory :hermit)
(defonce POS            (rsn/instances PartOfSpeech))           ; Collect for lookup
(defonce POS-Fragments  (into {} (map #(vector (:literal (check-fact % hasPartOfSpeechTag))
                                               (iri-fragment %))
                                      POS)))


;;; --------------------------------------------------------------------------
(defn lookup
  "Returns the PartOfSpeech entity corresponding to the specified tag.

  NOTES:
    - You probably want to use the (equivalent) memoized lookup# function.
    - The precomputed POS-Fragments map is an option if you need the POS name."
  [tag]
  (when-not (contains? (cfg/?? :cmu-pos :ignore) tag)

    ;; The caller may be in another namespace, but we need this namespace's ontology
    (binding [*ns* (find-ns 'say.cmu-pos)]

      ;; Run through the set of POS enitities until we find a matching tag
      (reduce #(when (= tag (:literal (check-fact %2 hasPartOfSpeechTag)))
                 (reduced %2))
              nil
              POS))))

(def lookup# (memoize lookup))

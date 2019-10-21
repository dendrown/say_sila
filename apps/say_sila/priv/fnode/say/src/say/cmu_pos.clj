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
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.cmu-pos
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.dolce          :as dul]
            [say.log            :as log]
            [say.social         :as soc]
            [clojure.string     :as str]
            [tawny.reasoner     :as rsn]
            [tawny.repl         :as repl]                       ; <= DEBUG
            [tawny.owl          :refer :all])
  (:import  [weka.core DenseInstance
                       Instance
                       Instances]
            [weka.filters.unsupervised.attribute TweetNLPPOSTagger
                                                 TweetToSentiStrengthFeatureVector]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/cmu-pos.owl#")
(def ^:const ONT-FPATH  "resources/KB/cmu-pos.owl")

(defonce TAGS {;Nominal, Nominal + Verbal
               "CommonNoun"                 ["N" "a common noun (NN,NNS)"]
               "Pronoun"                    ["O" "a pronoun (personal/WH, not possessive; PRP,WP)"]
               "NominalPossessive"          ["S" "a nominal + possessive"]
               "ProperNoun"                 ["^" "a proper noun (NNP,NNPS)"]
               "ProperNounPlusPossessive"   ["Z" "a proper noun + possessive"]
               "NominalVerbal"              ["L" "a nominal + verbal"]
               "ProperNounPlusVerbal"       ["M" "a proper noun + verbal"]
               ; Open-class words
               "Verb"                       ["V" "a verb including copula and auxiliaries (V*,MD)"]
               "Adjective"                  ["A" "an adjective (J*)"]
               "Adverb"                     ["R" "an adverb (R*,WRB)"]
               "Interjection"               ["!" "an interjection (UH)"]
               ; Closed-class words
               "Determiner"                 ["D" "a determiner (WDT,DT,WP$,PRP$)"]
               "CoordinatingConjunction"    ["&" "a coordinating conjunction (CC)"]
               "VerbParticle"               ["T" "a verb particle (RP)"]
               "ExistentialThere"           ["X" "an existential there, predeterminers (EX,PDT)"]
               "ExistentialPlusVerbal"      ["Y" "X+ a verbal"]
               "Preposition"                ["P" (str "a pre- or postposition, "
                                                      "or subordinating conjunction (IN,TO)")]
               ; Twitter/social media
               "Hashtag"                    ["#" "a hashtag (indicates a topic/category for a tweet)"]
               "Address"                    ["U" "a URL or email address"]
               "Emoticon"                   ["E" "an emoticon"]
               "AtMention"                  ["@" (str "an at-mention (indicating "
                                                      "another user as a recipient of a tweet)")]
               "Continuation"               ["~" (str "an indicator that the message continues "
                                                      "across multiple tweets")]
               ; Miscellaneous
               "Numeral"                    ["$" "a numeral (CD)"]
               "Punctuation"                ["," "punctuation"]
               "Other"                      ["G" (str "abbreviations, foreign words, possessive endings, "
                                                      "symbols, or garbage (FW,POS,SYM,LS)")]})


;;; --------------------------------------------------------------------------
(defontology cmu-pos
  :iri    ONT-IRI
  :prefix "pos")
(owl-import dul/dul)

(defclass PartOfSpeech
  :super   dul/InformationObject
  :label   "Part of Speech"
  :comment (str "An Information Object representing an Entity's part of speech"
                " according to CMU's tagging system."))

(defdproperty hasTag
  :domain  dul/InformationEntity
  :range   :XSD_STRING
  :comment "Defines a descriptive tag used to refer to an Information Entity"
  :characteristic :functional)


;; CMU POS tags are described in \cite{gimpel2011}
(doseq [[pos [tag descr]] TAGS]
  (individual pos
    :type    PartOfSpeech
    :label   (str/join " " (soc/tokenize pos))
    :fact    (is hasTag tag)
    :comment (str "A Part-of-Speech representing " descr)))

;;; --------------------------------------------------------------------------
(rsn/reasoner-factory :hermit)
(defonce POS (rsn/instances (get-domain cmu-pos hasTag)))   ;; Pre-collect for lookup


;;; --------------------------------------------------------------------------
(defn lookup
  "Returns the PartOfSpeech entity corresponding to the specified tag.
  NOTE: You probably want to use the (equivalent) memoized lookup# function."
  [tag]
  (reduce #(when (= tag (:literal (check-fact %2 hasTag)))
             (reduced %2))
          nil
          POS))

(def lookup# (memoize lookup))

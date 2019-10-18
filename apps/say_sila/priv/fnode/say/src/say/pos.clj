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
(ns say.pos
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.dolce          :as dul]
            [say.log            :as log]
            [tawny.repl         :as repl]                       ; <= DEBUG
            [tawny.owl          :refer :all])
  (:import  [weka.core DenseInstance
                       Instance
                       Instances]
            [weka.filters.unsupervised.attribute TweetNLPPOSTagger
                                                 TweetToSentiStrengthFeatureVector]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/pos.owl#")
(def ^:const ONT-FPATH  "resources/KB/pos.owl")

(defonce TAGS {;Nominal, Nominal + Verbal
               "N" "a common noun (NN,NNS)"
               "O" "a pronoun (personal/WH, not possessive; PRP,WP)"
               "S" "a nominal + possessive"
               "^" "a proper noun (NNP,NNPS)"
               "Z" "a proper noun + possessive"
               "L" "a nominal + verbal"
               "M" "a proper noun + verbal"
               ; Open-class words
               "V" "a verb including copula and auxiliaries (V*,MD)"
               "A" "an adjective (J*)"
               "R" "an adverb (R*,WRB)"
               "!" "an interjection (UH)"
               ; Closed-class words
               "D" "a determiner (WDT,DT,WP$,PRP$)"
               "P" "a pre- or postposition, or subordinating conjunction (IN,TO)"
               "&" "a coordinating conjunction (CC)"
               "T" "a verb particle (RP)"
               "X" "an existential there, predeterminers (EX,PDT)"
               "Y" "X+ a verbal"
               ; Twitter/social media
               "#" "a hashtag (indicates a topic/category for a twwet)"
               "@" "an at-mention (indicating another user as a recipient of a tweet)"
               "~" "an indicator that the message continues across multiple tweets"
               "U" "a URL or email address"
               "E" "an emoticon"
               ; Miscellaneous
               "$" "a numeral (CD)"
               "," "punctuation"
               "G" (str "abbreviations, foreign words, possessive endings, symbols,"
                        " or garbage (FW,POS,SYM,LS)")})


;;; --------------------------------------------------------------------------
(defontology pos
  :iri    ONT-IRI
  :prefix "pos")
(owl-import dul/dul)

(defclass PartOfSpeechTag
    :super   dul/InformationObject
    :label   "Part of Speech Tag"
    :comment "An Information Object representing an Entity's part of speech.")

;; CMU POS tags are described in \cite{gimpel2011}
(doseq [[ind descr] TAGS]
  (individual ind
    :type  PartOfSpeechTag
    :label (str "A Part-of-Speech Tag representing " descr)))

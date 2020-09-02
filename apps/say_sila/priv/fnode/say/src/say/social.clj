;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Utilities relating to social media
;;;;
;;;; @copyright 2019-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.social
  (:require [say.genie      :refer :all]
            [say.log        :as log]
            [clojure.string :as str]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Token-Splitter
             #"_|\-|\s|(?=\p{Lu}\p{Ll})|(?<=\p{Ll})(?=\p{Lu})|(?<=\p{L})(?=\p{N})|(?<=\p{N})(?=\p{L})")
             ;  white |  Upper->lower  |   lower-to-Upper    |  letter-to-digit  |  digit-to-letter


;;; --------------------------------------------------------------------------
(defn tokenize
  "Attempts to split up a username or similar into component tokens."
  [txt & opts]
  (as-> (filter seq (str/split txt Token-Splitter)) toks
    (if (some #{:lower-case} opts) (map str/lower-case toks)        toks)
    (if (some #{:upper-case} opts) (map str/upper-case toks)        toks)
    (if (some #{:str}        opts) (apply str (interpose " " toks)) toks)))



;;; --------------------------------------------------------------------------
(defn unhashtag
  "Removes the initial hashtag ( # ) character from the specified token if it
  has one."
  [tok]
  (if (= \# (first tok))
      (subs tok 1)
      tok))


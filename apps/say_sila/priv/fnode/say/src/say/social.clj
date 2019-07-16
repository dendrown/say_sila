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
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.social
  (:require [say.genie      :refer :all]
            [say.log        :as log]
            [clojure.string :as str]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const TOKEN-SPLITTER
             #"_|\s|(?=\p{Lu}\p{Ll})|(?<=\p{Ll})(?=\p{Lu})|(?<=\p{L})(?=\p{N})|(?<=\p{N})(?=\p{L})")
             ;white|  Upper->lower  |   lower-to-Upper    |  letter-to-digit  |  digit-to-letter


;;; --------------------------------------------------------------------------
(defn tokenize
  "
  Attempts to split up a username or similar into component tokens.
  "
  [txt & opts]
  (let [tokens (filter seq (str/split txt TOKEN-SPLITTER))]
    (if (some #{:lower-case} opts)
        (map str/lower-case tokens)
        tokens)))


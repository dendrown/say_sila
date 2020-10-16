;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Labels and identifiers for Say-Sila
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.label
  (:require [say.genie          :refer :all]
            [say.log            :as log]
            [say.cmu-pos        :as pos]
            [weka.dataset       :as dset]
            [clojure.string     :as str])
  (:import  (weka.core Instance)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const TWEET-TAG  "t")                        ; Tweet individual have this tag plus the ID ( "t42" )

(defonce Columns (dset/columns :s))                 ; Weka format for Say-Sila status/sentiment feed


;;; --------------------------------------------------------------------------
(defprotocol Polarizer
  "Determines negative|positive polarity for various datatypes."
  (polarize [x] "Return the sentiment polarity as :positive or :negative."))

(extend-protocol Polarizer
  Number
  (polarize [x]
    ;(log/debug "polarize:" x)
    (if (Double/isNaN x)
      :?
      (if (<= x 0.0)
          :negative
          :positive)))

  Instance
  (polarize [inst]
    (if (.classIsMissing inst)
        :?
        (polarize (.classValue inst))))


  String
  (polarize [pn]
    (polarize (Integer/parseInt pn))))



;;; --------------------------------------------------------------------------
(defn label-polarity
  "Returns the String «pos» or «neg» according to the polarity of x."
  [x]
  (label-polarity [x]
    (case (polarize x)
      :negative "neg"
      :positive "pos")))



;;; --------------------------------------------------------------------------
(defprotocol Labeller
  "Creates labels for say-senti purposes."
  ;; TODO: label-polarity [from above]
  (label-text [x] "Returns a String identifier for a Text (tweet)."))

(extend-protocol Labeller

  Instance
  (label-text [inst]
    (label-text (.value inst (int (Columns :id)))))

  String
  (label-text [s]
    ;; Allow multiple calls without retagging
    (if (str/starts-with? s TWEET-TAG)
        s
        (str TWEET-TAG s)))

  Object
  (label-text [x]
    (str TWEET-TAG (longify x))))



(defn label-text-token
  "Returns a String identifier for a Text (tweet) and the token number.

  Examples:
    - for the tenth token on the 42nd tweet, the label will be: t42-10
    - and for a MWE (multi-word expression) role on that token: t42-mwe10"
  ([txt tok]
  (hyphenize (label-text txt) tok))


  ([txt tok role]
  (hyphenize (label-text txt) (str (str/lower-case (name role)) tok))))


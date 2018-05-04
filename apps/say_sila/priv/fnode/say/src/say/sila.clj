;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Say-Sila ontology
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.sila
  (:require [say.foaf         :as foaf]
            [say.sioc         :as sioc]
            [clojure.java.io  :as io]
            [tawny.english    :as dl]
            [tawny.reasoner   :as rsn]
            [tawny.owl :refer :all])
    (:import  [org.semanticweb.owlapi.model IRI
                                          OWLOntologyID]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-sila")
(def ^:const ONT-FPATH  "resources/KB/say-sila.owl")


;;; --------------------------------------------------------------------------
(defontology say-sila
  :iri    ONT-IRI
  :prefix "sila")

(rsn/reasoner-factory :hermit)

; Top level:
;
; Building on sioc:Post ⊑ foaf:Document
(defclass Tweet
  :super   sioc/Post
  :label   "Tweet"
  :comment "A Twitter message post")

; Building on sioc:Role
(defclass Influencer
  :super    sioc/Role
  :disjoint Tweet
  :label    "Influencer"
  :comment  "User (not necessarily active) who affects other users' behaviour during a Say-Sila tracking run")

(defclass Player
  :super    sioc/Role
  :disjoint Tweet
  :label    "Player"
  :comment  "Active participant during a Say-Sila tracking run")

; Building on sioc:UserAccount
(defclass TwitterAccount
  :super    sioc/UserAccount
  :disjoint Tweet
  :label    "Twitter Account"
  :comment  "A user account on Twitter")


; Building on sioc:Post==>sila:Tweet
(as-subclasses Tweet
  :cover
  :disjoint
  (defclass Retweet
    :label      "Retweet"
    :comment    "A reposted twitter communication, originally written by someone else")
  (defclass OriginalTweet
    :equivalent (dl/not Retweet)    ; Seems backwards, but retweeted posts are tagged as such
    :label      "Original Tweet"
    :comment    "A twitter communication, posted by its original author"))


; Building on sioc:Role==>sila:Player
(as-subclasses Player
  :cover
  :disjoint
  (defclass BigPlayer
    :label   "Big Player"
    :comment "A Player (participant) who is extremely active during a tracking run")
  (defclass RegularPlayer
    :label   "Regular Player"
    :comment "A Player (participant) who demonstrates normal activity during a tracking run"))


; Building on sioc:UserAccount==>sila:TwitterAccount
(as-subclasses TwitterAccount
  :cover                        ; but not disjoint
  (defclass Author
    :label   "Author"
    :comment "A Twitter account from the viewpoint of posting tweets and communicating ideas")
  (defclass Tweeter
    :label   "Tweeter"
    :comment "A Twitter account, considered from the viewpoint of publishing tweets"))


; Building on sioc:UserAccount==>sila:TwitterAccount==>sila:Author
(as-subclasses Author
  :cover                        ; but not disjoint
  (defclass OriginalAuthor
    :label   "Original Author"
    :comment "A Twitter account responsible for writing an original tweet")
  (defclass MentionedAuthor
    :label   "Mentioned Author"
    :comment "A Twitter account who was mentioned in another user's tweet")
  (defclass RetweetedAuthor
    :label   "Retweeted Author"
    :comment "A Twitter account whose tweet has been republished by another tweeter"))


; Building on sioc:UserAccount==>sila:TwitterAccount==>sila:Tweeter
(as-subclasses Tweeter
  :cover                        ; but not disjoint
  (defclass OriginalTweeter
    :label   "Original Tweeter"
    :comment "A Twitter account that sends a tweet for which s/he is the original author")
  (defclass Retweeter
    :label   "Retweeter"
    :comment "A Twitter account that republishes a tweet originally authored by a different user"))



;;; --------------------------------------------------------------------------
(defn save
  "
  Saves the say-sila ontology in OWL format.
  "
  ([] (save ONT-FPATH))

  ([^String fpath]
  (save-ontology say-sila fpath :owl)))


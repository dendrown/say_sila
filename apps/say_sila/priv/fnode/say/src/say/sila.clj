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
            [say.log          :as log]
            [clojure.java.io  :as io]
            [tawny.english    :as dl]
            [tawny.reasoner   :as rsn]
            [tawny.owl :refer :all])
    (:import  [org.semanticweb.owlapi.model IRI
                                          OWLOntologyID]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const NS         *ns*)
(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-sila")
(def ^:const ONT-FPATH  "resources/KB/say-sila.owl")


;;; --------------------------------------------------------------------------
(defontology say-sila
  :iri    ONT-IRI
  :prefix "sila")

(rsn/reasoner-factory :hermit)


;;; Top level:
;;;
;;; TBox: building on sioc:Post ⊑ foaf:Document
(owl-class sioc/Post
  :super foaf/Document)

(defclass Tweet
  :super   sioc/Post
  :label   "Tweet"
  :comment "A Twitter message post")

;;; TBox: building on sioc:Role
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

;;; TBox: building on sioc:UserAccount
(defclass TwitterAccount
  :super    sioc/UserAccount
  :disjoint Tweet
  :label    "Twitter Account"
  :comment  "A user account on Twitter")


;;; TBox: building on sioc:Post==>sila:Tweet
(as-subclasses Tweet
  :cover
  :disjoint
  (defclass Retweet
    :label      "Retweet"
    :comment    "A reposted twitter communication, originally written by someone else")
  (defclass OriginalTweet
    :equivalent (dl/and Tweet
                        (dl/not Retweet))   ; Seems backwards, but retweeted posts get tagged
    :label      "Original Tweet"
    :comment    "A twitter communication, posted by its original author"))


;;; TBox: building on sioc:Role==>sila:Player
(as-subclasses Player
  :cover
  :disjoint
  (defclass BigPlayer
    :label   "Big Player"
    :comment "A Player (participant) who is extremely active during a tracking run")
  (defclass RegularPlayer
    :label   "Regular Player"
    :comment "A Player (participant) who demonstrates normal activity during a tracking run"))


;;; TBox: building on sioc:UserAccount==>sila:TwitterAccount
(as-subclasses TwitterAccount
  :cover                        ; but not disjoint
  (defclass Author
    :label   "Author"
    :comment "A Twitter account from the viewpoint of posting tweets and communicating ideas")
  (defclass Tweeter
    :label   "Tweeter"
    :comment "A Twitter account, considered from the viewpoint of publishing tweets"))


; TBox: building on sioc:UserAccount==>sila:TwitterAccount==>sila:Author
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


;;; TBox: building on sioc:UserAccount==>sila:TwitterAccount==>sila:Tweeter
(as-subclasses Tweeter
  :cover                        ; but not disjoint
  (defclass   OriginalTweeter
    :label    "Original Tweeter"
    :comment  "A Twitter account that sends a tweet for which s/he is the original author")

  (defclass Retweeter
    :label   "Retweeter"
    :comment "A Twitter account that republishes a tweet originally authored by a different user"))


;;; Roles: Careful, even attempting to reduce ambiguity as much as possible,
;;;        the language still gets tricky!
;;;
;;; tweeting:
(as-inverse
  (defoproperty tweets      :domain Tweeter :range Tweet)
  (defoproperty isTweetedBy :domain Tweet   :range Tweeter))

(refine OriginalTweeter :equivalent (dl/and Tweeter (dl/some tweets      OriginalTweet)))
(refine OriginalTweet   :equivalent (dl/and Tweet   (dl/some isTweetedBy OriginalTweeter)))
(refine OriginalAuthor  :equivalent OriginalTweeter)    ; OriginalAuthors are included only for
                                                        ; completeness.  Tracking runs do not
                                                        ; consider Tweet authors that are not
                                                        ; active Tweeters, retweeted, or mentioned.

;;; retweeting:
(as-inverse
  (defoproperty postsRetweetIn    :domain Retweeter :range Retweet   :super tweets)
  (defoproperty isRetweetPostedBy :domain Retweet   :range Retweeter :super isTweetedBy))

(as-inverse
  (defoproperty isRetweetBy   :domain Retweet         :range RetweetedAuthor)
  (defoproperty isRetweetedIn :domain RetweetedAuthor :range Retweet))

(as-inverse
  (defoproperty retweets
    :domain   Tweeter
    :range    RetweetedAuthor
    :subchain [postsRetweetIn isRetweetBy])
  (defoproperty isRetweetedBy
    :domain   RetweetedAuthor
    :range    Tweeter
    :subchain [isRetweetedIn isRetweetPostedBy]))

(refine Retweeter       :equivalent (dl/and Tweeter (dl/some postsRetweetIn Retweet)))
(refine RetweetedAuthor :equivalent (dl/and Author  (dl/some isRetweetedIn Retweet)))


;;; mentioning:
(as-inverse
  (defoproperty makesMentionIn :domain Tweeter :range  Tweet)
  (defoproperty hasMentionBy   :domain Tweet   :range  Tweeter))

(as-inverse
  (defoproperty hasMentionOf  :domain Tweet           :range  MentionedAuthor)
  (defoproperty isMentionedIn :domain MentionedAuthor :range  Tweet))

(as-inverse
  (defoproperty mentions
    :domain   Tweeter
    :range    MentionedAuthor
    :subchain [makesMentionIn hasMentionOf])
  (defoproperty isMentionedBy
    :domain   MentionedAuthor
    :range    Tweeter
    :subchain [isMentionedIn hasMentionBy]))


(refine MentionedAuthor :equivalent (dl/and Author
                                            (dl/some isMentionedIn Tweet)))



;;; --------------------------------------------------------------------------
(defn log-role
  "
  Returns a string to indicate a domain==role==>range operation.
  "
  [role dom rng]
  (log/info (log/<> 'IND *ns*) (str dom "--[" role "]--" rng)))



;;; --------------------------------------------------------------------------
(defn- make-individual
  "
  Defines an individual in the ontology.
  "
  [form]
  (try
    ;(log/debug form)
    (eval form)
    (catch Exception ex (log/fail "Cannot create individual:"))))



;;; --------------------------------------------------------------------------
(defmulti do-command
  "
  Processes the ontology command per the incoming map
  "
  :oproperty)

(defmethod do-command "tweets"
  [cmd]
  (let [{dom  :domain
         rng  :range} cmd
        tid           (str "t" rng)]        ; Make ID var-able
    (log-role "tweet" dom rng)
    (doseq [form [`(defindividual ~(symbol dom) :type Tweeter)
                  `(defindividual ~(symbol tid) :type Tweet)]]
        (make-individual form))))



(defmethod do-command "retweets"
  [cmd]
  (let [{dom  :domain
         rng  :range} cmd
        tid           (str "t" rng)]        ; Make ID var-able
    (log-role "retweet" dom rng)
    (doseq [form [`(defindividual ~(symbol dom) :type Reweeter)
                  `(defindividual ~(symbol tid) :type Retweet)]]
        (make-individual form))))



;;; --------------------------------------------------------------------------
(defn save
  "
  Saves the say-sila ontology to disk in OWL format.
  "
  ([] (save ONT-FPATH))

  ([^String fpath]
  (save-ontology say-sila fpath :owl)))


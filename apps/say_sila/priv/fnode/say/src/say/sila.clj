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
;;;; @copyright 2018-2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.sila
  (:require [say.bfo          :as bfo]
            [say.mfoem        :as mfoem]
            [say.foaf         :as foaf]
            [say.sioc         :as sioc]
            [say.log          :as log]
            [clojure.string   :as str]
            [clojure.java.io  :as io]
            [tawny.english    :as dl]
            [tawny.reasoner   :as rsn]
            [tawny.repl       :as repl]             ; <= DEBUG
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


;;; --------------------------------------------------------------------------
(defmacro redefclass
  "
  Add a class from another ontology namespace to the say-sila ontology
  and create variables to reference the class.
  "
  ([ns-var]
  (let [var (eval `(:name (meta #'~ns-var)))]
    `(redefclass ~(symbol var) ~ns-var)))


  ([var ns-var]
  `(do (def ~var ~ns-var)
       (refine ~var :label (str/replace (name '~var) #"-" " ")))))


;;; Top level:
;;;
;;; TBox: building on BFO
(owl-class bfo/continuant               :super bfo/entity)
(owl-class bfo/independent-continuant   :super bfo/continuant)
(owl-class bfo/material-entity          :super bfo/independent-continuant)
(owl-class mfoem/extended-organism      :super bfo/material-entity)
(owl-class mfoem/human-being            :super mfoem/extended-organism)

(redefclass bfo/entity)
(redefclass bfo/continuant)
(redefclass bfo/independent-continuant)
(redefclass bfo/material-entity)
(redefclass mfoem/extended-organism)


(defclass tester
  :super   mfoem/human-being
  :label   "tester"
  :comment "This is a class to test building on BFO.")


;;; DEPRECATED: we are moving off the foaf/sioc onto BFO-based ontologies
;;;
;;; TBox: building on foaf:Group ⊑ foaf:Agent
(owl-class foaf/Group
  (comment :super foaf/Agent))

(defclass AudienceSegment
  :super   foaf/Group
  :label   "Audience Segment"
  :comment "A potential target for an information campaign")

(as-subclasses AudienceSegment
  :cover
  :disjoint
  (defclass AlarmedSegment
    :label  "Alarmed Segment"
    :comment (str "People in the alarmed segement are sure anthropogenic climate change is occurring. "
                  "They support a strong response from government and enact changes in their own lives."))
  (defclass ConcernedSegment
    :label  "Concerned Segment"
    :comment (str "People in the concerned segment see climate change as a serious problem. "
                  "They support government initiatives, but generally do not take personal action."))
  (defclass CautiousSegment
    :label  "Cautious Segment"
    :comment (str "People in the cautious segment are not completely sure climate change exists, "
                  "though they do consider it a problem. They generally see no need for urgent action."))
  (defclass DisengagedSegment
    :label  "Disengaged Segment"
    :comment (str "People in the disengaged segment do not stay informed about climate change."
                  "They self-report as not knowing much on the subject."))
  (defclass DoubtfulSegment
    :label  "Doubtful Segment"
    :comment (str "People in the doubtful segment either do not think climmate change is happening; "
                  "they do not know; "
                  "or they believe it is due to natural causes, and there is no immediate danger."))
  (defclass DismissiveSegment
    :label  "Dismissive Segment"
    :comment (str "People in the dismissive segment generally do not believe sure climate change is happening. "
                  "They are actively engaged, but in opposition to people in the alarmed segment.")))



;;; TBox: building on sioc:Post ⊑ foaf:Document
(owl-class sioc/Post
  :super foaf/Document)

(defclass Survey
  :super   foaf/Document
  :label   "Survey"
  :comment "A series of questions intended to extract information from a group of people")

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
(defdproperty hasPostCount
  :domain Tweeter
  :range  :XSD_NON_NEGATIVE_INTEGER
  :characteristic :functional)

;;; retweeting:
(as-inverse
  (defoproperty postsRetweetIn    :domain Retweeter :range Retweet)
  (defoproperty isRetweetPostedBy :domain Retweet   :range Retweeter))

(as-inverse
  (defoproperty isRetweetFrom :domain Retweet         :range RetweetedAuthor)
  (defoproperty isRetweetedIn :domain RetweetedAuthor :range Retweet))

(as-inverse
  (defoproperty retweets
    :domain   Tweeter
    :range    RetweetedAuthor
    :subchain [postsRetweetIn isRetweetFrom])
  (defoproperty isRetweetedBy
    :domain   RetweetedAuthor
    :range    Tweeter
    :subchain [isRetweetedIn isRetweetPostedBy]))

(refine Retweeter       :equivalent (dl/and Tweeter (dl/some postsRetweetIn Retweet)))
(refine RetweetedAuthor :equivalent (dl/and Author  (dl/some isRetweetedIn  Retweet)))


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


;;; Big Players and Influencers
(refine Influencer :equivalent (dl/and Tweeter (dl/or (at-least 3 isRetweetedIn)
                                                      (at-least 3 isMentionedIn))))


;;; --------------------------------------------------------------------------
(defn log-role
  "
  Returns a string to indicate a domain==role==>range operation.
  "
  [role dom rng]
  (log/info (log/<> 'ROLE *ns*) (str dom "--[" role "]--" rng)))



;;; --------------------------------------------------------------------------
(defn- form->ontology
  "
  Evaluates a clojure form, presumably to add an entity to the ontology.
  "
  [form]
  (try
    ;(log/debug form)
    (eval form)
    (catch Exception ex (log/fail ex "Bad ontology form:"))))



;;; --------------------------------------------------------------------------
(defmulti alter-ontology
  "
  Processes the ontology command per the incoming map
  "
  (fn [prop _ _] prop))


(defmethod alter-ontology "tweets"
  [prop dom rng]
  (doseq [form [`(defindividual ~(symbol dom) :type OriginalTweeter)
               ;`(defindividual ~(symbol rng) :type OriginalTweet)  ; Keep the ontology small-ish
               ]]
    (form->ontology form)))


(defmethod alter-ontology "retweets"
  [prop dom rng]
  (doseq [form [`(defindividual ~(symbol rng) :type Retweet)
                `(defindividual ~(symbol dom) :type Retweeter
                                              :fact (is postsRetweetIn ~(symbol rng)))]]
    (form->ontology form)))


(defmethod alter-ontology "isRetweetFrom"
  [prop dom rng]
  (doseq [form [`(defindividual ~(symbol rng) :type Author)         ; reasoner => RetweetedAuthor
                `(defindividual ~(symbol dom) :type Retweet
                                              :fact (is isRetweetFrom ~(symbol rng)))]]
    (form->ontology form)))


(defmethod alter-ontology "makesMentionIn"
  [prop dom rng]
  (doseq [form [`(defindividual ~(symbol rng) :type Tweet)
                `(defindividual ~(symbol dom) :type Tweeter
                                              :fact (is makesMentionIn ~(symbol rng)))]]
    (form->ontology form)))


(defmethod alter-ontology "hasMentionOf"
  [prop dom rng]
  (doseq [form [`(defindividual ~(symbol rng) :type Author)         ; reasoner => MentionedAuthor
                `(defindividual ~(symbol dom) :type Tweet
                                              :fact (is hasMentionOf ~(symbol rng)))]]
    (form->ontology form)))


(defmethod alter-ontology "hasPostCount"
  [prop dom rng]
  (doseq [form [`(defindividual ~(symbol dom) :type Tweeter
                                              :fact (is hasPostCount ~rng))]]
    (form->ontology form)))


(defmethod alter-ontology :default
  [prop dom rng]
  (log/warn (log/fmt "Unrecognized request: prop[~a] dom[~a] rng[~a]" prop dom rng)))



;;; --------------------------------------------------------------------------
(defn execute
  "
  Processes a sequence of ontology commands from say_sila/Erlang
  "
  [cmds]
  (doseq [cmd cmds]
    (let [{prop :property           ; oproperty|dproperty
           dom   :domain
           rng   :range} cmd]
      (log-role prop dom rng)
      (alter-ontology prop dom rng))))



;;; --------------------------------------------------------------------------
(defn save
  "
  Saves the say-sila ontology to disk in OWL format.
  "
  ([] (save ONT-FPATH))

  ([^String fpath]
  (log/info "Saving ontology:" fpath)
  (save-ontology say-sila fpath :owl)))


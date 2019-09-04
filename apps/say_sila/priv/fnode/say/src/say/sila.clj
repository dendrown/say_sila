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
  (:refer-clojure :exclude [==])
  (:require [say.ontology       :refer :all]
            [say.config         :as cfg]
            [say.dolce          :as dul]
            [say.foaf           :as foaf]
            [say.sioc           :as sioc]
            [say.log            :as log]
            [clojure.string     :as str]
            [clojure.java.io    :as io]
            [clojure.pprint     :as prt :refer [pp pprint]]
            [tawny.english      :as dl]
            [tawny.reasoner     :as rsn]
            [tawny.query        :as qry]
            [tawny.repl         :as repl]                   ; <= debug
            [tawny.owl          :refer :all]
            [clojure.core.logic :refer :all :exclude [annotate is]])
  (:import  [org.semanticweb.owlapi.model   IRI
                                            OWLOntologyID]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const NS         *ns*)
(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-sila")
(def ^:const ONT-FPATH  "resources/KB/say-sila.owl")


;;; --------------------------------------------------------------------------
;;; TODO: we have a number of decisions that are not yet final...
(def ^:const FOAF?      true)


;;; --------------------------------------------------------------------------
(defontology say-sila
  :iri    ONT-IRI
  :prefix "sila")

(rsn/reasoner-factory :hermit)


;;; --------------------------------------------------------------------------
;;; Top level:
;;;
;;; Imports foundational & reference ontologies
(doseq [imp (filter some? [dul/dul
                           (when FOAF? foaf/foaf)])]
  (owl-import imp))

;;; Top-level ontology: Dolce+D&S Ultralite
(defcopy dul/Agent)
(defcopy dul/Concept)
(defcopy dul/Person)
(defcopy dul/Organization)

(defcopy dul/associatedWith)
(defcopy dul/isMemberOf)

(defclass Tester
  :super   dul/Person
  :label   "Tester"
  :comment "This is a class to test building on DOLCE")


;;; --------------------------------------------------------------------------
;;; Demographics:
;;;
;;; TODO: make final decision on whether or not to utilise FOAF
(if FOAF?
  ;; TODO: Evaluating HCLS/POMR Ontology (predecessor of Bio-zen plus \cite{samwald2008})
  (do
    (refine Agent :equivalent foaf/Agent)
    (defcopy foaf/gender)

    (def Female "FEMALE")
    (def Male   "MALE"))

  ;; TODO: Evaluating Gender ⊑ dul/Quality
  (do
    (defclass Gender
      :super    dul/Quality
      :label    "Gender"
      :comment  (str "The Quality of being a specific biological sex and/or being part of the corresponding"
                     "social group"))

    (defindividual Female
      :type     Gender
      :label    "Female"
      :comment  (str "The Gender associated with having female reproductive organs and/or "
                     "fulfilling a feminine role in society."))

    (defindividual Male
      :type     Gender
      :label    "Male"
      :comment  (str "The Gender associated with having male reproductive organs and/or "
                     "fulfilling a masculine role in society."))

    (defoproperty gender                            ; TODO: Change to isOfGender if !FOAF
      :super    associatedWith
      :domain   Agent
      :range    Gender
      :characteristic :functional)))


;;; --------------------------------------------------------------------------
;;; Object Properties
(defoproperty hasRole
  :super    associatedWith
  :label    "has Role"
  :domain   dul/Agent
  :range    dul/Role)

(defoproperty supports
  :super    associatedWith
  :label    "supports"
  :domain   (dl/or dul/Agent
                   dul/Role)
  :range    Concept)


;;; --------------------------------------------------------------------------
;;; Concepts:
(defindividual Environmentalism
  :type     Concept
  :label    "Environmentalism"
  :comment  "The Concept of caring about the evironment and supporting evironmentally-friendly policies.")

;;; --------------------------------------------------------------------------
;;; Roles:
(defindividual Environmentalist
  :type     dul/Role
  :label    "Environmentalist"
  :comment  "The Role of someone involved in Environmentalism"
  :fact     (is supports Environmentalism))


;;; --------------------------------------------------------------------------
;;; Politics
;;;
;;; NOTE: We may be moving towards \cite{porello2014} for modelling social groups
;;;
;;; TODO: Find and cite source of definitions for political terms
(defclass PoliticalIdeology
  :super    Concept
  :label    "Political Ideology"
  :comment  "The Concept concerning the ideals of a political system.")


(defindividual Conservatism
  :type     PoliticalIdeology
  :label    "Conservatism"
  :comment  "The Political Ideology which seeks to preserve traditional political and social institutions.")


(defindividual Liberalism
  :type     PoliticalIdeology
  :label    "Liberalism"
  :comment  "The Political Ideology which supports equality and civil liberty.")


(defindividual Moderatism
  :type     PoliticalIdeology
  :label    "Moderatism"
  :comment  (str "The Political Ideology which supports neither Conservativism, nor Liberalism "
                 "and whose supporters often consider themselves in the middle of both of these ideologies."))


(defclass PoliticalParty
  :super   dul/Collective
  :label   "Political Party"
  :comment "An Organization that represents a group of Persons with similar political ideals.")

(defindividual DemocraticParty
  :type PoliticalParty
  :comment "The Political Party representing the Left in the United States.")


(defindividual RepublicanParty
  :type PoliticalParty
  :comment "The Political Party representing the Right in the United States.")

;;; FIXME: Indendent is not a political party...!
(defindividual Independent
  :type     PoliticalParty
  :label    "Independent Political Ideology"
  :comment  (str "The Political Party of no-party "
                 "whose members often consider themselves in the middle of Republicans and Democrats. "
                 "NOTE: We are addressing the fact that independents do not constitute a true Political Party."))




;;; --------------------------------------------------------------------------
;;; Six Americas
(defclass AudienceSegment
  :super   dul/Collective
  :label   "Audience Segment"
  :comment "A collective that is a potential target for an information campaign")

(defoproperty inAudienceSegment :domain Person :range AudienceSegment)

(as-subclasses AudienceSegment
  :cover
  ;:disjoint
  (defclass AlarmedSegment
    :label  "Alarmed Segment"
    :comment (str "An Audience Segment of people "
                  "who are sure anthropogenic climate change is occurring, "
                  "who support a strong response from government and "
                  "who enact changes in their own lives."))
  (defclass ConcernedSegment
    :label  "Concerned Segment"
    :comment (str "An Audience Segment of people "
                  "who see climate change as a serious problem and "
                  "who support government initiatives but generally do not take personal action."))
  (defclass CautiousSegment
    :label  "Cautious Segment"
    :comment (str "An Audience Segment of people "
                  "who are not completely sure climate change exists and "
                  "who generally see no need for urgent action, "
                  "though they do consider it a problem."))
  (defclass DisengagedSegment
    :label  "Disengaged Segment"
    :comment (str "An Audience Segment of people "
                  "who do not stay informed about climate change and"
                  "who self-report as not knowing much on the subject."))
  (defclass DoubtfulSegment
    :label  "Doubtful Segment"
    :comment (str "An Audience Segment of people "
                  "who either do not think climmate change is happening; "
                  "who do not know; or "
                  "who believe it is due to natural causes and that there is no immediate danger."))
  (defclass DismissiveSegment
    :label  "Dismissive Segment"
    :comment (str "An Audience Segment of people "
                  "who generally do not believe sure climate change is happening and "
                  "who are actively engaged but in opposition to people in the alarmed segment.")))


;;; We will be comparing Twitter users to prototypes of the Audience Segments.
;;; Qualities and values are presented in segment Person Prototypes.
;;; TODO: Make the Person Prototype classes distinct
(defindividual AlarmedPersonPrototype
  :type     AlarmedSegment
  :label    "Alarmed Person Prototype"
  :comment  "A hypothetical member of the Alarmed Segment who embodies all qualities of that Audience Segment."
  :fact     (is gender Female)                                          ; 61%
            (is isMemberOf DemocraticParty)                             ; 58%
            (is supports Liberalism)                                    ; 48%
            (is hasRole Environmentalist))

(defindividual ConcernedPersonPrototype
  :type     ConcernedSegment
  :label    "Concerned Person Prototype"
  :comment  "A hypothetical member of the Concerned Segment who embodies all qualities of that Audience Segment."
  :fact     (is gender Female)                                          ; 52% (remove?)
            (is isMemberOf DemocraticParty)                             ; 47%
            (is supports Moderatism)                                    ; 45%
            (is hasRole Environmentalist))                              ; "somewhat"

(defindividual CautiousPersonPrototype
  :type     CautiousSegment
  :label    "Cautious Person Prototype"
  :comment  "A hypothetical member of the Cautious Segment who embodies all qualities of that Audience Segment."
  :fact     (is gender Male)                                            ; 53% (remove?)
            (is supports Moderatism))                                   ; 40%

(defindividual DisengagedPersonPrototype
  :type     DisengagedSegment
  :label    "Disengaged Person Prototype"
  :comment  "A hypothetical member of the Disengaged Segment who embodies all qualities of that Audience Segment."
  :fact     (is gender Female)                                          ; 62%
            (is isMemberOf DemocraticParty)                             ; 41%
            (is supports Moderatism)                                    ; 44%
            (dl/not hasRole Environmentalist))

(defindividual DoubtfulPersonPrototype
  :type     DoubtfulSegment
  :label    "Doubtful Person Prototype"
  :comment  "A hypothetical member of the Doubtful Segment who embodies all qualities of that Audience Segment."
  :fact     (is gender Male)                                            ; 59%
            (is isMemberOf RepublicanParty)                             ; 56%
            (is supports Conservatism)                                  ; 61%
            (dl/not hasRole Environmentalist))

(defindividual DismissivePersonPrototype
  :type     DismissiveSegment
  :label    "Dismissive Person Prototype"
  :comment  "A hypothetical member of the Dismissive Segment who embodies all qualities of that Audience Segment."
  :fact     (is gender Male)                                            ; 63%
            (is isMemberOf RepublicanParty)                             ; 64%
            (is supports Conservatism)                                  ; 75%
            (dl/not hasRole Environmentalist))


;;; TODO FOAF/SIOC are no longer primary as we move onto DOLCE-based ontologies
;;;
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

(defdproperty hasMaleTweetCount
  :domain Tweeter
  :range  :XSD_NON_NEGATIVE_INTEGER
  :characteristic :functional)

(defdproperty hasFemaleTweetCount
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
(defn form->ontology
  "
  Evaluates a clojure form, presumably to add an entity to the ontology.
  "
  [[call ent & args]]
  ;; These forms usually end up calling def in tawny.owl/intern-owl. We are
  ;; likely here because of a call from another namespace, but Clojure doesn't
  ;; allow the use of def to define variables in one namespace from another.
  (binding [*ns* (find-ns 'say.sila)]
    ;; We're specifying the ontology explicitly since we're juggling namespaces.
    ;; Tawny OWL requires that the ontology come before other (frame) arguments.
    ;; Destructuring gives us the Tawny macro call and the entity.  Now we need
    ;; to reconstruct the list form, adding back the head elements in reverse order.
    (let [form (conj args `say-sila :ontology (symbol ent) call)]
      (log/debug form)
      (try
        (eval form)
        (catch Exception ex (log/fail ex "Bad ontology form" :stack))))))



;;; --------------------------------------------------------------------------
(defmulti alter-ontology
  "
  Processes the ontology command per the incoming map
  "
  (fn [prop _ _] prop))


(defmethod alter-ontology "tweets"
  [prop dom rng]
  (doseq [form [`(defindividual ~dom :type OriginalTweeter)
               ;`(defindividual ~rng :type OriginalTweet)  ; Keep the ontology small-ish
               ]]
    (form->ontology form)))


(defmethod alter-ontology "retweets"
  [prop dom rng]
  (doseq [form [`(defindividual ~rng :type Retweet)
                `(defindividual ~dom :type Retweeter
                                     :fact (is postsRetweetIn rng))]]
    (form->ontology form)))


(defmethod alter-ontology "isRetweetFrom"
  [prop dom rng]
  (doseq [form [`(defindividual ~rng :type Author)         ; reasoner => RetweetedAuthor
                `(defindividual ~dom :type Retweet
                                     :fact (is isRetweetFrom ~rng))]]
    (form->ontology form)))


(defmethod alter-ontology "makesMentionIn"
  [prop dom rng]
  (doseq [form [`(defindividual ~rng :type Tweet)
                `(defindividual ~dom :type Tweeter
                                     :fact (is makesMentionIn ~rng))]]
    (form->ontology form)))


(defmethod alter-ontology "hasMentionOf"
  [prop dom rng]
  (doseq [form [`(defindividual ~rng :type Author)         ; reasoner => MentionedAuthor
                `(defindividual ~dom :type Tweet
                                     :fact (is hasMentionOf ~rng))]]
    (form->ontology form)))


(defmethod alter-ontology "hasPostCount"
  [prop dom rng]
  (doseq [form [`(defindividual ~dom :type Tweeter
                                     :fact (is hasPostCount ~rng))]]
    (form->ontology form)))


(defmethod alter-ontology :default
  [prop dom rng]
  (log/warn (log/fmt "Unrecognized request: prop[~a] dom[~a] rng[~a]" prop dom rng)))



;;; --------------------------------------------------------------------------
(defn get-gender-tweet-counts
  "Returns a map of the counts of tweets for the specifed Tweeter that were
  classified as :male and :female."
  [tweeter]
  {:female  (get-count tweeter hasFemaleTweetCount 0)
   :male    (get-count tweeter hasMaleTweetCount   0)})



;;; --------------------------------------------------------------------------
(defn execute
  "Processes a sequence of ontology commands from say_sila/Erlang."
  [cmds]
  (doseq [cmd cmds]
    (let [{prop :property           ; oproperty|dproperty
           dom   :domain
           rng   :range} cmd]
      (log-role prop dom rng)
      (alter-ontology prop dom rng))))



;;; --------------------------------------------------------------------------
(defn save
  "Saves the say-sila ontology to disk in OWL format."
  ([] (save ONT-FPATH))

  ([^String fpath]
  (log/info "Saving ontology:" fpath)
  (save-ontology say-sila fpath :owl)))


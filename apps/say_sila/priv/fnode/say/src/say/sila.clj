;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; The say-sila ontology and associtated functionality.
;;;;
;;;; @copyright 2018-2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.sila
  (:refer-clojure :exclude [==])
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [say.dolce          :as dul]
            [say.foaf           :as foaf]
            [say.cmu-pos        :as pos]
            [say.senti          :as senti]
            [weka.core          :as weka]
            [weka.dataset       :as dset]
            [clojure.string     :as str]
            [clojure.java.io    :as io]
            [clojure.pprint     :refer [pp]]
            [defun.core         :refer [defun]]
            [tawny.english      :as dl]
            [tawny.reasoner     :as rsn]
            [tawny.query        :as qry]
            [tawny.repl         :as repl]                   ; <= debug
            [tawny.owl          :refer :all]
            [clojure.core.logic :refer :all :exclude [annotate is]])
  (:import  (org.semanticweb.owlapi.model   IRI
                                            OWLOntology
                                            OWLOntologyID)
            (weka.core Instance)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-ISTUB  "http://www.dendrown.net/uqam/say-sila")
(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/say-sila.owl#")
(def ^:const ONT-FPATH  "resources/KB/say-sila.owl")
(def ^:const ONT-FSTUB  "resources/KB/say-sila")

(def ^:const INIT-DATA  {:tag :env, :tracker :all, :source :tweets, :dir "resources/emo-sa"})

(defonce World          (atom {:users {}
                               :texts {}
                               :ontology {}}))


;;; --------------------------------------------------------------------------
(defontology say-sila
  :iri    ONT-IRI
  :prefix "sila")

(rsn/reasoner-factory :hermit)


;;; --------------------------------------------------------------------------
;;; Top level:
;;;
;;; We use the DOLCE+DnS Ultralite (DUL) foundational ontology.  However, to
;;; avoid making multiple class declarations, we use a chain of imports.
;;;
;;; say-sila <- say-senti <- cmu-pos <- DUL
;;;
;;; (This issue may be due to Tawny-OWL's co-maintaining RDF/XML and Clojure
;;; variables in namespaces representing the various ontologies.)
(owl-import senti/say-senti)


;;; --------------------------------------------------------------------------
;;; Environmental clues at the Text level
;;;
;;; TBox: building on pos:Token

(defmacro def-affect-token
  "Creates an affect Token class for the given (String) sentiment polarity or emotion."
  [aff]
  `(defclass ~(symbol (str aff "Token"))
     :super    pos/Token
     :label    (str ~aff " Token")
     :comment  (str "A Token which may indicate " ~aff ".")
     :equivalent (dl/and pos/Token
                         (dl/some senti/denotesAffect (owl-class senti/say-senti ~aff)))))

;;; Create affect Token and Information Objects for all defined polarities and emotions
;(run! #(def-affect-token %) senti/Affect-Names)                ; FIXME
(def-affect-token "Positive")
(def-affect-token "Negative")
(def-affect-token "Anger")
(def-affect-token "Fear")
(def-affect-token "Sadness")
(def-affect-token "Joy")
(def-affect-token "Surprise")
(def-affect-token "Anticipation")
(def-affect-token "Disgust")
(def-affect-token "Trust")

(defmacro def-affect-info-obj
  "Creates an affect InformationObject class for the given (String) sentiment polarity or emotion."
  [aff]
  `(defclass ~(symbol (str aff "InformationObject"))
    :super    dul/InformationObject
    :label    (str ~aff " Information Object")
    :comment  (str "An Information Object which may indicate " ~aff ".")
    :equivalent (dl/and dul/InformationObject
                        (dl/some dul/hasComponent ~(symbol (str aff "Token"))))))

;(run! #(def-affect-info-obj %) senti/Affect-Names)         ; FIXME
(def-affect-info-obj "Positive")
(def-affect-info-obj "Negative")
(def-affect-info-obj "Anger")
(def-affect-info-obj "Fear")
(def-affect-info-obj "Sadness")
(def-affect-info-obj "Joy")
(def-affect-info-obj "Surprise")
(def-affect-info-obj "Anticipation")
(def-affect-info-obj "Disgust")
(def-affect-info-obj "Trust")


(defmacro def-affect-pos-token
  "Creates an affect Part-of-Speech class for the given (String) sentiment polarity or emotion."
  [aff pos]
  `(defclass ~(symbol (str aff pos "Token"))
     :super    (owl-class (str ~aff "Token"))
     :label    (str ~aff " " ~pos "Token")
     :comment  (str "A " ~pos " Token which may indicate " ~aff ".")
     :equivalent (dl/and pos/Token
                         (dl/some pos/isPartOfSpeech (owl-class pos/cmu-pos ~pos))
                         (dl/some senti/denotesAffect (owl-class senti/say-senti ~aff)))))

(defmacro define-affect-pos-tokens!
  "Creates an affect Part-of-Speech classes for the say-sila ontology."
  []
  (conj (for [aff senti/Affect-Names
              pos ["CommonNoun" "Verb"]]
          `(def-affect-pos-token ~aff ~pos))
        'do))

(define-affect-pos-tokens!)


;;; --------------------------------------------------------------------------
(defclass SurveyKeyword
  :super    pos/Token
  :label    "Survey Keyword"
  :comment  "A Token which is considered to be a keyword in a Six America's survey."
  :equivalent (dl/and pos/Token
                      (dl/some dul/isComponentOf senti/Survey)))

(defclass SurveyReference
  :super    dul/InformationObject
  :label    "Survey Reference"
  :comment  (str "An Information Object which has one or more keywords from  a "
                 "Six Americas survey.")
  :equivalent (dl/and dul/InformationObject
                      (dl/some dul/hasComponent SurveyKeyword)))


;;; --------------------------------------------------------------------------
;;; Demographics:
;;;
;;; TBox: building on dul:SocialObject
(comment
(defclass TwitterAccount
  :super    senti/OnlineAccount
  :label    "Twitter Account"
  :comment  "A user account on Twitter")

(comment defclass Influencer
  :super    senti/OnlineAccount
  :label    "Influencer"
  :comment  "User (not necessarily active) who affects other users' behaviour during a Say-Sila tracking run")

(defclass Player
  :super    senti/OnlineAccount
  :label    "Player"
  :comment  "Active participant during a Say-Sila tracking run")

(as-subclasses Player
  :cover
  :disjoint
  (defclass BigPlayer
    :label   "Big Player"
    :comment "A Player (participant) who is extremely active during a tracking run")
  (defclass RegularPlayer
    :label   "Regular Player"
    :comment "A Player (participant) who demonstrates normal activity during a tracking run"))
);comment


;;; --------------------------------------------------------------------------
;;; TODO: Evaluating Gender ⊑ dul/Quality
(comment
(defclass Gender
  :super    dul/Quality
  :label    "Gender"
  :comment  (str "The Quality of being a specific biological sex and/or being part of the corresponding"
                 "social group"))

(as-subclasses Gender
  ;; NOTE: we are (for the moment) being insensitive to non-binary-indentifying persons
  ;;       in an effort to create a working ontological system.  With apologies to anyone
  ;;       asserting that the covers/disjoint keys here are do not reflect today's reality,
  ;;        we shall reconsider these definitions at a future time.
  :cover
  :disjoint
  (defclass FemaleGender
    :label    "Female Gender"
    :comment  (str "The Gender associated with having female reproductive organs and/or "
                   "fulfilling a feminine role in society."))

  (defclass MaleGender
    :label    "Male Gender"
    :comment  (str "The Gender associated with having male reproductive organs and/or "
                   "fulfilling a masculine role in society.")))

(defpun FemaleGender)
(defpun MaleGender)

(defoproperty isOfGender
  :super    dul/hasQuality
  :domain   dul/Person
  :range    Gender
  :characteristic :functional)
);comment

;;; --------------------------------------------------------------------------
;;; Object Properties
(comment defoproperty supports
 ;:super    dul/associatedWith                  % FIXME: needs DUL hierarchy
  :label    "supports"
  :domain   (dl/or dul/Agent
                   dul/Role)
  :range    dul/Concept)


;;; --------------------------------------------------------------------------
;;; Concepts:
(comment defindividual Environmentalism
  :type     dul/Concept
  :label    "Environmentalism"
  :comment  "The Concept of caring about the evironment and supporting evironmentally-friendly policies.")


;;; --------------------------------------------------------------------------
;;; Roles:
(comment defindividual Environmentalist
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
(comment
(defclass PoliticalIdeology
  :super    dul/Concept
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
);comment


;;; --------------------------------------------------------------------------
;;; Six Americas
(comment
(defclass AudienceSegment
  :super   dul/Collective
  :label   "Audience Segment"
  :comment "A collective that is a potential target for an information campaign")

(defoproperty inAudienceSegment :domain dul/Person :range AudienceSegment)

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
  :fact     (is isOfGender theFemaleGender)                             ; 61%
            (is dul/isMemberOf DemocraticParty)                         ; 58%
            (is supports Liberalism)                                    ; 48%
            (is dul/hasRole Environmentalist))

(defindividual ConcernedPersonPrototype
  :type     ConcernedSegment
  :label    "Concerned Person Prototype"
  :comment  "A hypothetical member of the Concerned Segment who embodies all qualities of that Audience Segment."
  :fact     (is isOfGender theFemaleGender)                             ; 52% (remove?)
            (is dul/isMemberOf DemocraticParty)                         ; 47%
            (is supports Moderatism)                                    ; 45%
            (is dul/hasRole Environmentalist))                          ; "somewhat"

(defindividual CautiousPersonPrototype
  :type     CautiousSegment
  :label    "Cautious Person Prototype"
  :comment  "A hypothetical member of the Cautious Segment who embodies all qualities of that Audience Segment."
  :fact     (is isOfGender theMaleGender)                               ; 53% (remove?)
            (is supports Moderatism))                                   ; 40%

(defindividual DisengagedPersonPrototype
  :type     DisengagedSegment
  :label    "Disengaged Person Prototype"
  :comment  "A hypothetical member of the Disengaged Segment who embodies all qualities of that Audience Segment."
  :fact     (is isOfGender theFemaleGender)                             ; 62%
            (is dul/isMemberOf DemocraticParty)                         ; 41%
            (is supports Moderatism)                                    ; 44%
            (dl/not dul/hasRole Environmentalist))

(defindividual DoubtfulPersonPrototype
  :type     DoubtfulSegment
  :label    "Doubtful Person Prototype"
  :comment  "A hypothetical member of the Doubtful Segment who embodies all qualities of that Audience Segment."
  :fact     (is isOfGender theMaleGender)                               ; 59%
            (is dul/isMemberOf RepublicanParty)                         ; 56%
            (is supports Conservatism)                                  ; 61%
            (dl/not dul/hasRole Environmentalist))

(defindividual DismissivePersonPrototype
  :type     DismissiveSegment
  :label    "Dismissive Person Prototype"
  :comment  "A hypothetical member of the Dismissive Segment who embodies all qualities of that Audience Segment."
  :fact     (is isOfGender theMaleGender)                               ; 63%
            (is dul/isMemberOf RepublicanParty)                         ; 64%
            (is supports Conservatism)                                  ; 75%
            (dl/not dul/hasRole Environmentalist))
);comment


;;; TBox: building on dul:InformationObject==>senti:Text
(defclass PersonalProfile
  :super   dul/InformationObject
  :label   "Personal Profile"
  :comment "An Information Object consisting of a personal description for an online user.")

(as-disjoint PersonalProfile senti/Text pos/Token)

(comment
(defclass Tweet
  :super   senti/Text
  :label   "Tweet"
  :comment "A Twitter message status message.")

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


;;; TBox: building on senti:OnlineAccount==>sila:TwitterAccount
(as-subclasses TwitterAccount
  :cover                        ; but not disjoint
  (defclass Author
    :label   "Author"
    :comment "A Twitter account from the viewpoint of posting tweets and communicating ideas")
  (defclass Tweeter
    :label   "Tweeter"
    :comment "A Twitter account, considered from the viewpoint of publishing tweets"))


;;; TBox: building on senti:OnlineAccount==>sila:TwitterAccount==>sila:Author
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



;;; TBox: building on senti:OnlineAccount==>sila:TwitterAccount==>sila:Tweeter
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
);comment


;;; --------------------------------------------------------------------------
;;; Environmental clues at the Account level
;;;
;;; TBox: building on OnlineAccount ⊑ dul:SocialObject
(defclass GreenAccount
  :super    senti/OnlineAccount
  :label    "Green Account"
  :comment  "An Online Account that represents someone who is concerned about the environment."
  :equivalent (dl/and
                senti/OnlineAccount
                (dl/or ; [TODO] go beyond this initial SWAG
                  ;; Profile trigger
                  (dl/some dul/isReferenceOf (dl/and FearInformationObject
                                                     SurveyReference))
                  ;; Tweet trigger
                  (dl/and (dl/some senti/publishes FearInformationObject)
                          (dl/some senti/publishes SurveyReference)))))

(defclass RogueAccount
  :super    senti/OnlineAccount
  :label    "Rogue Account"
  :comment  "An Online Account which does not adhere to the rules of its associated online provider.")




;;; --------------------------------------------------------------------------
;;; Align our class hierarchy with FOAF if configured to do so
(when (cfg/?? :sila :foaf?)
  ;(owl-import foaf/foaf)

  ;; NOTE: Linking in the FOAF and SIOC ontologies is a bit problematic, considering
  ;;       how the Agent, Person, and Role classes differ somewhat from DUL.
  ;;       Compare with:
  ;;        - Human activity representation in smart-homes \cite{ni2016})
  ;;        - HCLS/POMR Ontology (predecessor of Bio-zen plus \cite{samwald2008})
  (refine dul/Agent   :equivalent foaf/Agent)
  (refine dul/Person  :equivalent foaf/Person)

  ;;; TBox: building on sioc:Post ⊑ foaf:Document
  (refine  dul/InformationObject :equivalent foaf/Document)
  (refine  senti/OnlineAccount   :equivalent foaf/OnlineAccount)
  (refine  PersonalProfile       :equivalent foaf/PersonalProfileDocument)

  ;; We're using a dul/Quality-based modelling for Gender
  (comment refine FemaleGender :equivalent (has-value foaf/gender foaf/Female))
  (comment refine MaleGender   :equivalent (has-value foaf/gender foaf/Male)))


;;; --------------------------------------------------------------------------
(defn log-role
  "Returns a string to indicate a domain==role==>range operation."
  [role dom rng]
  (log/info (log/<> 'ROLE *ns*) (str dom "--[" role "]--" rng)))



;;; --------------------------------------------------------------------------
(defn form->ontology
  "Evaluates a clojure form, presumably to add an entity to the ontology."
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
  "Processes the ontology command per the incoming map

  TODO: Tawny OWL has limited support for handling individuals using Strings.
        We're currently using its 'normal' methodology by which individuals
        are (also) instantiated as variables.  This is not ideal, as we need
        to handle 10s of thousands of individual Tweeters."
  (fn [prop _ _]
    (if (string? prop) prop (name prop))))


(defmethod alter-ontology "tweets"
  [prop dom rng]

 ;(let [dsym (symbol (dom))]
 ;  ;; Do we need to add the tweeter?
 ;  (when-not (and (resolve dsym
 ;                 (t/individual (eval dsym))))
 ;    (form->ontology `(defindividual ~dom :type OriginalTweeter)))

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
(comment defn get-gender-tweet-counts
  "Returns a map of the counts of tweets for the specifed Tweeter that were
  classified as :male and :female."
  [tweeter]
  ;; TODO: Streamline string<-->symbol handling
  (let [dom (if (string? tweeter)
                (eval (symbol tweeter))
                tweeter)]
    {:female  (get-count dom hasFemaleTweetCount 0)
     :male    (get-count dom hasMaleTweetCount   0)}))



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


;;; --------------------------------------------------------------------------
(defn ont-iri
  "Creates a (String) IRI to indentify a related say-sila. ontology with
  individuals from social media."
  [otag]
  (str ONT-ISTUB "-" (name otag) ".owl#"))



;;; --------------------------------------------------------------------------
(defn ^OWLOntology make-ontology
  "Creates a version (copy) of the say-sila ontology, intended to include
  individuals expressing the specified Sentiment Composition Rule (SCR)"
  [otag]
  (let [oname  (name otag)
        prefix #(apply str % "-" oname %&)]
    ;; We use a (sub)ontology to hold the texts and DL-Learner solutions
    (ontology
      :tawny.owl/name (prefix "say-sila")
      :iri     (ont-iri oname)
      :prefix  (prefix oname)
      :import  say-sila
      :comment (str "Ontology for modelling '" oname "'Twitter users and their activity."))))



;;; --------------------------------------------------------------------------
(defun ^OWLOntology populate-ontology
  "Populates an ontology using examples extracted from an ARFF with user data."
  ([xmps :guard map?]
  (update-kv-values xmps #(populate-ontology %1 %2)))


  ([dtag :guard keyword? xmps]
  (populate-ontology (make-ontology dtag) xmps))


  ([ont xmps]
  (run! (fn [{:as xmp
              sname :screen_name
              descr :description                ; User profile text
              tid   :tid }]                     ; Text ID is the profile ID
          (let [sconf (cfg/? :senti)                                    ; Use senti/text config params
                acct  (individual ont sname :type senti/OnlineAccount)  ; Twitter user account
                prof  (individual ont tid                               ; User profile
                                  :type PersonalProfile
                                  :fact (is dul/isAbout acct))]
            ;; Add PoS/senti for the profile content
            (senti/add-text ont prof xmp sconf)))
        xmps)
    ;; The (Java) ontology is mutable, return the updated version
    ont))



;;; --------------------------------------------------------------------------
(defn instances->examples
  "Converts Weka maps corresponding to the specified instances."
  ([data]
  ;; NOTE: Our Twitter user data (U00) is currently unlabeled.
  (let [target  :environmentalist
        insts   (weka/load-dataset data target)
        dset    (dset/Datasets :u)                              ; Structure for user (U99) data
        tools   (senti/toolbox)                                 ; Sentiment/emotion analysis
        attrs   (select-keys (dset/Columns dset) [:screen_name  ; 0-based attribute indices
                                                  :name
                                                  :description
                                                  :environmentalist])]
    ;; Create a sequence of maps, each representing a Weka instance
    (map #(update % target keyword)                             ; Lazily convert "?" to :?
         (reduce
           (fn [acc ^Instance inst]
             (let [avals (update-values attrs #(.stringValue inst (int %)))         ; Pull attr-vals
                   sname (:screen_name avals)
                   tid   (str "ProfileOf_" sname)                                   ; TextID is profile name
                   xmp   (senti/make-example tools tid sname (:description avals))] ; Check senti/emo
             ;; Add on hashmap with attribute data plus senti/emo analysis
             (conj acc (merge avals xmp))))
         '()
         (weka/instance-seq insts)))))


  ([dset data]
  (hash-map dset (instances->examples data))))



;;; --------------------------------------------------------------------------
(defn create-world!
  "Loads the official say-sila examples and ontologies."
  ([]
  ;; Pull the configured dataset information for users and their texts.
  ;; TODO: Incorporate dset/t->su
  (let [{dtag   :tag
         track  :tracker
         src    :source
         dir    :dir}   (cfg/?? :sila :data INIT-DATA)
         [users texts]  (map #(apply strfmt "~a/~a.~a.~a.~a.arff"
                                            (map name [dir src track dtag (dset/code %)]))
                             [:user :senti])]
    (create-world! dtag users texts)))


  ([dtag ausers atexts]
  ;; The tweet texts are handled in say.senti
  (log/fmt-info "Dataset user~a: ~a" dtag ausers)
  (log/fmt-info "Dataset text~a: ~a" dtag atexts)

  (let [xusers (instances->examples dtag ausers)
        xtexts (senti/instances->examples dtag atexts)
        world  (-> (populate-ontology dtag (xusers dtag))
                   (senti/populate-ontology (xtexts dtag))) ]

    ;; Set our top-level state. Each element holds a tagged map of the appropriate data
    (reset! World {:users    xusers
                   :texts    xtexts
                   :ontology {dtag world}})

    (keys xusers))))



;;; --------------------------------------------------------------------------
(defn report-world
  "Give positive/negative/emotion/survey/part-of-speech coverage for the
  Say-Sila World data. Passing a :users option will list the users for
  each data tag."
  [& opts]
  (let [world @World
        users (when (some #{:users} opts)               ; On request:
                (update-values (world :users)           ; Tagged maps of user sequences
                               #(into #{} (map :screen_name %))))]
    ;; Report affect and PoS in profiles and tweets
    (run! (fn [[elm title]]
            (log/info title)
            (senti/report-examples (world elm))
            (log/debug))
          [[:users "User Profiles:"]
           [:texts "User Tweets:"]])

    (doseq [[tag us] users]
      (log/debug)
      (log/fmt-info "USERS~a: ~a" tag us))))


;;; --------------------------------------------------------------------------
(defn save-ontologies
  "Saves the say-sila ontology and all World ontologies in OWL format."
  []
  (save-ontology say-sila ONT-FPATH :owl)
  (merge {:say-sila ONT-FPATH}
         (save-ontology-map (:ontology @World) ONT-FSTUB)))


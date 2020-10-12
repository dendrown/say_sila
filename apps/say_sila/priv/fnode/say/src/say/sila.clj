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
            [say.cmu-pos        :as pos]
            [say.community      :as comm]
            [say.dllearner      :as dll]
            [say.dolce          :as dul]
            [say.foaf           :as foaf]
            [say.infer          :as inf]
            [say.jvm            :as jvm]
            [say.label          :as lbl]
            [say.senti          :as senti]                  ; FIXME: deprecated
            [say.social         :as soc]
            [say.survey         :as six]
            [say.tweebo         :as twbo]
            [weka.core          :as weka]
            [weka.dataset       :as dset]
            [weka.tweet         :as tw]
            [clojure.edn        :as edn]
            [clojure.java.io    :as io]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp]]
            [defun.core         :refer [defun]]
            [tawny.english      :as dl]
            [tawny.reasoner     :as rsn]
            [tawny.query        :as qry]
            [tawny.repl         :as repl]                   ; <= debug
            [tawny.owl          :refer :all]
            [clojure.core.logic :refer :all :exclude [annotate is]])
  (:import  (java.util Random)
            (org.semanticweb.owlapi.model   IRI
                                            OWLOntology
                                            OWLOntologyID)
            (weka.core Instance
                       Instances)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Ont-IStub      "http://www.dendrown.net/uqam/say-sila")
(def ^:const Ont-IRI        "http://www.dendrown.net/uqam/say-sila.owl#")
(def ^:const Ont-FPath      "resources/KB/say-sila.owl")
(def ^:const Ont-FStub      "resources/KB/say-sila")
(def ^:const Emotion-FStub  "resources/world")
(def ^:const World-FStub    "resources/world")

(def ^:const Init-Data      {:tag :env, :tracker :all, :source :tweets, :dir Emotion-FStub})
(def ^:const Split-Tags     [:train :test])


;;; Expressions for Liu's sentiment composition rules (SCR)
(defonce Expressions    {})                                     ; Currently unused
(defonce Memory         (agent {:start (jvm/memory-used :MB)})) ; Memory used in megabytes

(defonce World          (atom {:users {}
                               :texts {}
                               :ontology {}}))

(defontology say-sila
  :iri    Ont-IRI
  :prefix "sila")

(rsn/reasoner-factory :hermit)


;;; --------------------------------------------------------------------------
;;; Top level:
;;;
;;; We use the DOLCE+DnS Ultralite (DUL) foundational ontology.  However, to
;;; avoid making multiple class declarations, we use a chain of imports.
;;;
;;; say-sila <- cmu-pos <- DUL
;;;
;;; (This issue may be due to Tawny-OWL's co-maintaining RDF/XML and Clojure
;;; variables in namespaces representing the various ontologies.)
(owl-import pos/cmu-pos)


;;; NOTE: Our model was initially based off work by Salguero and Espinilla \cite{salguero2016}.
;;;       They use the disjoint base classes: Term, Sentence, Document where Token and Punctuation
;;;       are subclasses of Term.  In our model, Punctuation is covered in cmu-pos, which leaves
;;;       Term and Token equivalent.  Additionally, following their model of disjoint classes here
;;;       causes a complexity explosion in the reasoner tableau.
(defclass Text
  :super    dul/InformationObject
  :disjoint pos/Token
  :label    "Text"
  :comment  "An Information Object consisting of text.")

;;; Keep the actual tweet/profile content as a development aid
(defaproperty TextualContent)

(defclass OnlineAccount
  :super    dul/SocialObject
  :label    "Online Account"
  :comment  "A user account for an online service.")

;;; Help DL-Learner to not confuse our primary entities
(as-disjoint OnlineAccount dul/InformationObject dul/Quality)


(defoproperty publishes
  :label    "publishes"
  :domain   OnlineAccount
  :range    dul/InformationObject
  :comment  "The action of making an Information Object available to an online community.")


;;; --------------------------------------------------------------------------
;;; Support for TweeboParser dependency trees?
(when (cfg/?? :sila :use-tweebo?)

  (defclass Coordination
    :super   dul/Concept
    :label   "Coordination"
    :comment "A linguistic Concept that links one or more Conjuncts.")

  (defclass Conjuncts
    :super   dul/Collection
    :label   "Conjuncts"
    :comment "A Collection that contains the set of conjuncts conjoined with a conjunction or coordination.")

  (defclass MultiWordExpression
    :super   dul/Collection
    :label   "Multi-word concept"
    :comment "A Collection of Tokens that together represent a single unit of meaning.")

   (as-inverse
    (defoproperty dependsOn
      :domain  dul/Entity
      :range   dul/Entity
      :label   "depends on"
      :comment "A relationship describing how one Entity's existence or correctness is contingent on another."
      :characteristic :transitive)

    (defoproperty hasDependent
      :domain  dul/Entity
      :range   dul/Entity
      :label   "has dependent"
      :comment "A relationship describing how another Entity's existence or correctness is contingent on this Entity."
      :characteristic :transitive))

  ;; dependsOn will need to be under associatedWith, depending on the DUL config setting
  (when (not= :minimal (dul/which-mode))
    (doseq [oprop [dependsOn
                   hasDependent]]
      (refine oprop
        :super dul/associatedWith)))

  (as-inverse dependsOn hasDependent)
  (as-inverse
    (defoproperty directlyDependsOn
      :super   dependsOn
      :domain  dul/Entity
      :range   dul/Entity
      :label   "directly depends on"
      :comment (str "A relationship describing how an Entity's existence or correctness is"
                    "immediately contingent on another."))

    (defoproperty hasDirectDependent
      :super   hasDependent
      :domain  dul/Entity
      :range   dul/Entity
      :label   "has direct dependent"
      :comment (str "A relationship describing how another Entity's existence or correctness is"
                    "immediately contingent on this one."))))


;;; --------------------------------------------------------------------------
;;; A Survey may be used to compare w/ analysis methods on social media
(defclass Survey
  :super   dul/InformationObject
  :label   "Survey"
  :comment "A series of questions intended to extract information from a group of people")

(defindividual sassy
  :type  Survey
  :label "SASSY"
  :comment "Six Americas Short SurveY")

(defindividual six36
  :type  Survey
  :label "Six Americas 36-Question Survey"
  :comment "Original Six Americas survey with 36 questions.")

(defonce Surveys        (select-keys {:sassy sassy                  ; Only configured surveys
                                      :six36 six36}
                                     (cfg/?? :sila :surveys #{})))
(defonce Survey-Names   (map name (keys Surveys)))


;;; --------------------------------------------------------------------------
(defclass Affect
  :super    dul/Concept
  :label    "Affect"
  :comment  "A concept describing human sentiment or emotion.")

(as-subclasses Affect
  (defclass Positive
    :label   "Positive"
    :comment "An affective concept representing positive sentiment polarity.")

  (defclass Negative
    :label   "Negative"
    :comment "An affective concept representing negative sentiment polarity."))

(run! #(defpun %) [Positive Negative])


(defoproperty denotesAffect
  :super   dul/expresses
  :label   "denotes affect"
  :domain  pos/Token
  :range   Affect
  :comment "A relationship between a Token and the affect it expresses.")


(defmacro defemotion
  "Adds a Concept reprenting an emotion to the say-sila ontology"
  [emo sys & combos]
  (let [ename   `(name '~emo)
        descr   `(str "A concept which expresses the class of human affect generally known as "
                       (str/lower-case ~ename)
                      " according to the system of base emotions by " (str/capitalize (name ~sys))
                      "."
                      (when-not (empty? '~combos)
                        (apply str " This emotion is a combination of the following base emotions: "
                                   (interpose ", " '~combos))))]

    `(do (defclass ~emo :super Affect, :label ~ename, :comment ~descr)
         (defpun ~emo))))


(defmacro defemotions
  "Adds all the emotions handled by a lexicon into the say-sila ontology."
  [sys]
  (let [esys  (eval sys)
        emote (fn [e] `(defemotion ~e ~esys))]
    (when esys
      ;; Create Affect concepts according to the system
      (conj (case esys
              :plutchik (map emote '[Anger Fear ,, Sadness Joy        ,, Surprise Anticipation ,, Disgust Trust])
              :ekman    (map emote '[Anger Fear    Sadness Happiness     Surprise                 Disgust])
              `((log/fmt-error "Unsupported emotion system: [~a]" ~esys)))

            `(log/fmt-info "Creating base emotion set: [~a]" ~esys)       ; Building a do-expr in reverse!
            'do))))

;;; Create Emotions in the ontology per the configured emo-system
(defonce Emotion-System (cfg/?? :sila :emo-system))
(defemotions Emotion-System)

(defonce Affect-Fragments   (into {} (map #(let [a (iri-fragment %)]
                                             [(lower-keyword a) a])
                                           (rsn/instances Affect))))
(defonce Affect-Names       (into #{} (vals Affect-Fragments)))


;;; We must declare the different types of Aspect to be disjoint for the reasoner
;;; to handle equivalency classes based on the complement of a given Aspect.
(apply as-subclasses Affect :disjoint (map #(owl-class %) Affect-Names))


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
                         (dl/some denotesAffect (owl-class say-sila ~aff)))))

;;; Create affect Token and Information Objects for all defined polarities and emotions
;(run! #(def-affect-token %) Affect-Names)                      ; FIXME
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

;(run! #(def-affect-info-obj %) Affect-Names)                   ; FIXME
(comment
;; FIXME: Looking into issues with DL-Learner never returning when it has too many
;;        [Affect][PoS]InformationObjects to play with.
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
)

;;; --------------------------------------------------------------------------
;;; Combinations of Affect PLUS Part-of-Speech
(defmacro def-affect-pos-element
  "Creates an affect Part-of-Speech Token class for the given (String) sentiment polarity or emotion."
  [aff pos base equiv]
  `(let [pos-label#  ~(soc/tokenize pos  :str)
         base-label# ~(soc/tokenize base :str)]
     ;; Put all the pieces together for the combo-class
     (defclass ~(symbol (str aff pos base))
       :super    (owl-class ~(str aff base))
       :label    (str ~aff " " pos-label# " " base-label#)
       :comment  (str "A " pos-label# " " base-label# " which may indicate " ~aff ".")
       :equivalent ~equiv)))


(defmacro def-affect-pos-token
  "Creates an affect Part-of-Speech Token class for the given (String) sentiment polarity or emotion."
  [aff pos]
  `(def-affect-pos-element ~aff ~pos "Token"
     (dl/and (owl-class ~(str aff "Token"))
             (dl/some pos/isPartOfSpeech (owl-class pos/cmu-pos ~pos)))))


(defmacro def-affect-pos-info-obj
  "Creates an affect Information Object class for the given (String) sentiment polarity or emotion."
  [aff pos]
  `(def-affect-pos-element ~aff ~pos "InformationObject"
     (dl/and (owl-class ~(str aff "InformationObject"))
             (dl/some dul/hasComponent ~(symbol (str aff pos "Token"))))))


(defmacro def-affect-pos-classes
  "Creates an affect Part-of-Speech classes for the say-sila ontology."
  [poss]
  ;; Define a Token AND InformationObject for all Affect on the PoS elements
  `(do ~@(for [aff Affect-Names
               pos (eval poss)]
           `(do (def-affect-pos-token    ~aff ~pos)
                ;; FIXME: Looking into issues with DL-Learner never returning when it has too many
                ;;        [Affect][PoS]InformationObjects to play with.
                (comment def-affect-pos-info-obj ~aff ~pos)))))

(def-affect-pos-classes ["CommonNoun" "Verb"])


;;; --------------------------------------------------------------------------
(defclass SurveyReferenceRule
  :super   dul/Concept
  :label   "Survey Reference Rule"
  :comment (str "An abstraction describing a Text or portion of a Text, "
                 "indicating that it refers to a question from a Six Americas survey."))


;;; Are we using specialized object properties?
(defoproperty indicatesRule
  :super   dul/expresses
  :label   "indicates rule"
  :domain  pos/Token
  :range   SurveyReferenceRule
  :comment "A relationship between a Token and the survey reference rule it expresses.")


(defmacro defrule
  "Adds a Sentiment Composition Rule (component) subclass to the say-sila ontology"
  [tag descr]
  `(do (defclass ~tag
         :super   SurveyReferenceRule
         :label   (str "Survey Reference Rule - " (name '~tag))
         :comment ~descr)
       (defpun ~tag)))

;;; Concept indictor rules
(defrule CAUSE  "Expressions which indicate a causal relationship.")
(defrule HUMAN  "Expressions which refer to humans or humanity.")
(defrule NATURE "Expressions which refer to the natural world.")

(defrule NEGATION "Expressions which negate other terms.")

;; HermiT gives us all kinds of problems at inference-time if we don't
;; specifically identify negated and non-negated (affirmed) Token types.
(defclass NegationToken
  :label "Negation Token"
  :comment "A Token that negates one or more (other) Tokens in its Information Object."
  :super pos/Token)

(defclass NotNegatedCheck
  :label "Not Negated Check"
  :comment "A syntactic dependency test affirming that a Tokens has not been negated."
  :super dul/Concept)
(defpun NotNegatedCheck)


;;; --------------------------------------------------------------------------
(defclass SurveyKeyword
  :super    pos/Token
  :label    "Survey Keyword"
  :comment  "A Token which is considered to be a keyword in a Six America's survey."
  :equivalent (dl/and pos/Token
                      (dl/some dul/isComponentOf Survey)))

(comment defclass SurveyReference   ; TODO: Pending decision on info-objs
  :super    dul/InformationObject
  :label    "Survey Reference"
  :comment  (str "An Information Object which has one or more keywords from  a "
                 "Six Americas survey.")
  :equivalent (dl/and dul/InformationObject
                      (dl/some dul/hasComponent SurveyKeyword)))


(comment defclass BeliefsQuestionKeyword    ; TODO: Tie questions in with keyword concepts
  :super    SurveyKeyword
  :label    "Beliefs Question Keyword"
  :comment  "A Keyword which is refers to the question on beliefs (Table 5) in the Six America's survey.")


(when (cfg/?? :sila :use-tweebo?)
  (defclass HumanCauseToken
    :super pos/Token
    :equivalent (dl/and pos/Token
                        (dl/or
                          (dl/and
                            (dl/some indicatesRule HUMAN)
                            (dl/some indicatesRule CAUSE))
                          (dl/and
                            (dl/some indicatesRule HUMAN)
                            (dl/some dependsOn (dl/some indicatesRule CAUSE))))))

  (defclass NaturalCauseToken
    :super pos/Token
    :equivalent (dl/and pos/Token
                        (dl/or
                          (dl/and
                            (dl/some indicatesRule NATURE)
                            (dl/some indicatesRule CAUSE))
                          (dl/and
                            (dl/some indicatesRule NATURE)
                            (dl/some dependsOn (dl/some indicatesRule CAUSE))))))

  ;; Dependency analysis allows us to know whether or not human-cause tokens are negated
  (as-disjoint
    (defclass NegatedHumanCauseToken
      :super HumanCauseToken
      :equivalent (dl/and HumanCauseToken
                          (dl/some hasDependent NegationToken)))

    (defclass AffirmedHumanCauseToken
      :super HumanCauseToken
      :equivalent (dl/and HumanCauseToken
                          (dl/some directlyDependsOn NotNegatedCheck))))

  ;; Natural-cause indicators follow the same pattern
  (as-disjoint
    (defclass NegatedNaturalCauseToken
      :super NaturalCauseToken
      :equivalent (dl/and NaturalCauseToken
                          (dl/some hasDependent NegationToken)))

    (defclass AffirmedNaturalCauseToken
      :super NaturalCauseToken
      :equivalent (dl/and NaturalCauseToken
                          (dl/some directlyDependsOn NotNegatedCheck))))

  (defclass HumanCauseBelieverAccount
    :super OnlineAccount
    :equivalent (dl/and OnlineAccount
                        (dl/or
                          (dl/some publishes (dl/some dul/hasComponent AffirmedHumanCauseToken))
                          (dl/some publishes (dl/some dul/hasComponent NegatedNaturalCauseToken)))))

  (defclass NaturalCauseBelieverAccount
    :super OnlineAccount
    :equivalent (dl/and OnlineAccount
                        (dl/or
                          (dl/some publishes (dl/some dul/hasComponent AffirmedNaturalCauseToken))
                          (dl/some publishes (dl/some dul/hasComponent NegatedHumanCauseToken))))))


;;; --------------------------------------------------------------------------
;;; Demographics:
;;;
;;; TBox: building on dul:SocialObject
(comment
(defclass TwitterAccount
  :super    OnlineAccount
  :label    "Twitter Account"
  :comment  "A user account on Twitter")

(comment defclass Influencer
  :super    OnlineAccount
  :label    "Influencer"
  :comment  "User (not necessarily active) who affects other users' behaviour during a Say-Sila tracking run")

(defclass Player
  :super    OnlineAccount
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

(as-disjoint PersonalProfile Text pos/Token)

(comment
(defclass Tweet
  :super   Text
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
(def ^:const DLL-Denier
 "publishes some (hasComponent some (AngerToken and (FearToken or SadnessToken))) (pred. acc.: 76.67%, F-measure: 58.82%)")

(defclass DenierAccount
  :super    OnlineAccount
  :label    "Denier Account"
  :comment  "An Online Account that represents someone who does not believe in anthropogenic climate change."
  :equivalent (dl/and
                OnlineAccount
                (tawny.english/some publishes
                    (tawny.english/some say.dolce/hasComponent
                           (tawny.english/and AngerToken
                           (tawny.english/or FearToken SadnessToken))))))

(defclass GreenAccount
  :super    OnlineAccount
  :label    "Green Account"
  :comment  "An Online Account that represents someone who is concerned about the environment.")

(defclass RogueAccount
  :super    OnlineAccount
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
  (refine  OnlineAccount         :equivalent foaf/OnlineAccount)
  (refine  PersonalProfile       :equivalent foaf/PersonalProfileDocument)

  ;; We're using a dul/Quality-based modelling for Gender
  (comment refine FemaleGender :equivalent (has-value foaf/gender foaf/Female))
  (comment refine MaleGender   :equivalent (has-value foaf/gender foaf/Male)))


;;; --------------------------------------------------------------------------
;;; Tell DL-Learner about our ontology elements
(dll/register-ns)


;;; --------------------------------------------------------------------------
(def     Polarity-Markers   {"Negative" (str log/Lt-Blue   "--")
                             "Positive" (str log/Lt-Yellow "++")
                             :?         (str log/White     "??")})

(def     Emotion-Colours    {"Anger"        log/Red1
                             "Fear"         log/Green3
                             "Joy"          log/Gold1
                             "Sadness"      log/Dk-Violet
                             "Anticipation" log/Orange3
                             "Surprise"     log/Cyan1
                             "Trust"        log/SpringGreen1
                             "Disgust"      log/Magenta2})

(defn elegend
  "Returns a legend for emotion to colour mapping."
  []
  (map (fn [[emo col]]
         (str col emo log/Text))
       (merge Emotion-Colours
              Polarity-Markers)))



;;; --------------------------------------------------------------------------
(defn eword
  "Returns a printable colour-coded string of word high-lighted with respect
  to the specified sentiment/emotion set."
  ([word affect]
  (eword word affect nil nil))


  ([word affect survey]
  (eword word affect survey (tw/make-stemmer)))


  ([word affect survey sball]
  (let [smark (when (and survey (six/in-survey? survey word sball))
                log/Underline)]
    ;; Make sure we have something to process
    (if (every? empty? [affect smark])
      word
      (let [; Surround the word with colour-coded pos/neg markers
            [prefix
             suffix] (reduce (fn [[pri suf :as acc]
                                  [pole code]]
                               (if (contains? affect pole)
                                   [(str code pri)
                                    (str suf code)]
                                   acc))
                             ["" ""]
                             Polarity-Markers)
            ; Colourize the letters in the word according to the emotions
            colours (vals (select-keys Emotion-Colours affect))
            ccnt    (count colours)
            weave   (if (zero? ccnt)
                      (str smark word)
                      ;; Pad word for an even split across emotions
                      (let [wlen    (count word)
                            csize   (Math/ceil (/ wlen ccnt))
                            padding (take (- (* ccnt csize) wlen)       ; Extra chars required
                                          (repeat \*))                  ; Asterix, not Obelix
                            [pada
                             padz]  (split-at (quot (count padding) 2) padding) ; Extra @ end: *word**
                            chunks  (partition-all csize (concat pada
                                                                 (seq word)
                                                                 padz))]
                        ;; Weave emotion colours into word!
                        (apply str (flatten (interleave (repeat smark) colours chunks)))))]

        ;; End the colourized word must by going back to no colour
        (str prefix log/Text
             weave  log/Text
             suffix log/Text))))))


;;; --------------------------------------------------------------------------
(defn etweet
  "Returns a colourized string representing the example tweet. Survey keyword
  hits are underlined.  The function defaults to using the configured survey,
  or SASSY if none is configured.)"
  ([xmp]
  ;;; TODO: Handle multiple surveys if we're going to support more than one
  (etweet xmp (six/which-survey)))


  ([{:keys [tid
            polarity
            content
            analysis]}
    survey
    & opts]
  ;; The analysis includes sentiment, emotion, and SCRs. The latter are ignored.
  (let [snowball  (when survey
                    (tw/make-stemmer))
        colourize (fn [[word affect]]
                    (eword word affect survey snowball))
        pn-code   (Polarity-Markers (Affect-Fragments polarity polarity))
        etokens   (map colourize (zip content analysis))                ; Mark affect
        etext     (apply str (interpose \space (conj etokens            ; Tag tweet
                                                     (log/<> tid pn-code))))]
    ;; The default return is a string tagged with the ID, but they may want the tokens
    (if (some #{:map} opts)
        {:etokens etokens,
         :etext   etext}
        etext))))



;;; --------------------------------------------------------------------------
(defn eprint-tweet
  "Prints a colourized representions of a tweet and its token dependency tree."
  [xmp]
  (let [exmp (etweet xmp (six/which-survey) :map)]
    (println (:etext exmp))
    (twbo/print-tree (merge xmp exmp))
    (println)))



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
(defn ont-iri
  "Creates a (String) IRI to indentify a related say-sila. ontology with
  individuals from social media."
  [otag]
  (str Ont-IStub "-" (name otag) ".owl#"))



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
(defprotocol OntologyFactory
  "Functionality to create individual ontologies."
  (make-ontology-maker [o]  "Returns a factory function which, depending on
                             the :sila :community configuration setting, either
                             returns a single ontology or a new one for each user."))

(extend-protocol OntologyFactory
  OWLOntology
  (make-ontology-maker [ont]
    ;; Simple function to always return the same "all users" ontology
    (fn [& _ ]
      ont))


  clojure.lang.IFn
  (make-ontology-maker [onter]
    ;; Assume the function is already an ontology factory
    onter)


  clojure.lang.Keyword
  (make-ontology-maker [otag]
    (make-ontology-maker (name otag)))


  String
  (make-ontology-maker [otag]
    (if (cfg/?? :sila :community?)
      ;; Each user has an individual ontology
      (let [onter (fn [sname]
                    (make-ontology (apply hyphenize otag sname)))]

        ;; Function to retrieve/create user ontologies in the community
        (fn [& sname]
          (comm/fetch sname onter)))

      ;; All users share a single ontology
      (make-ontology-maker (make-ontology otag)))))



;;; --------------------------------------------------------------------------
(defn meaningful?
  "Returns true if a token is 'meaningful' as per the given :sila
  (sub)configuration map and the specified concept sequences."
  [sconf & concepts]
  (or (:all-tokens? sconf)
      (not-every? empty? concepts)))



;;; --------------------------------------------------------------------------
(defn add-text
  "Adds a textual individual to the specified ontology.  The default behaviour
  is to create a new individual of type Text, but the arity-4 clause allows
  the caller to specify any entity needing to represent a series of Tokens."
  ([onter tinfo]
  (add-text onter tinfo (cfg/? :sila)))


  ([onter tinfo sconf]
  (add-text onter nil tinfo sconf))


  ([onter entity
    {:keys [affect content tid pos-tags rules screen_name surveys]}     ; Text breakdown
    {:keys [full-links? links? use-tweebo?]                             ; Senti-params
     :as   sconf}]
  ;; The code will assume there's at least one token, so make sure!
  (when (seq pos-tags)
    (let [ont   (onter screen_name)                                 ; Combined|individual ontology
          msg   (apply str (interpose " " content))
          text  (or entity
                    (individual ont tid                             ; Entity representing the text
                      :type Text))]                                 ; Determine textual type

    ;; Annotate the actual text content as a development aid
    (refine ont text :annotation (annotation TextualContent msg))

    ;; If they didn't pass an entity, assume this is a tweet
    ;; TODO: Handle access to say.sila namespace
    (when (and screen_name                                              ; Test data may not have screen names
               (not entity))
      (refine ont (individual ont screen_name) :fact (is publishes text)))

     ;; Prepare for Tweebo Parsing if desired
     (when use-tweebo?
       (twbo/prepare tid msg))

      ;; And entities for each of the terms, linking them together and to the text
      (reduce
        (fn [[cnt tokens]
             [aff scr tag word svys]]
          ;; Get the Part of Speech for the tag reported by Weka
          (if-let [pos (and (meaningful? sconf aff scr svys)
                            (pos/lookup# tag))]

            ;; Set up an individual for this Token.
            ;;
            ;; NOTE: We have to declare the individual using its Token subclass type
            ;;       (Negated or Affirmed), but these subclasses are declared later.
            (let [ttid  (str tid "-" cnt)
                  curr  (individual ont ttid
                                    :type  (if (some #{"NEGATION"} scr)
                                               NegationToken
                                               pos/Token)
                                    :label (str ttid " ( " tag " / " word " )"))]

              ;; Link Token to the original Text and set POS Quality
              (refine ont text :fact (is dul/hasComponent curr))
              (refine ont curr :fact (is pos/isPartOfSpeech pos))

            ;; Link tokens to each other
            (when-let [prev (first tokens)]

              ;; Are we including the Token ordering?
              (when links?
                (refine ont curr :fact (is dul/directlyFollows prev))

                ;; The reasoner can figure out the rest, but being explicit may be faster
                (when full-links?
                  ;; The current Token comes after all the tokens we've seen so far
                  (refine ont prev :fact (is dul/directlyPrecedes curr))
                  (run! (fn [tok]
                          (refine ont curr :fact (is dul/follows tok))
                          (refine ont tok  :fact (is dul/precedes curr)))
                        tokens))))

            ;; Express sentiment/emotion
            (doseq [a aff]
              (refine ont curr :fact (is denotesAffect (individual say-sila a))))

            ;; Express sentiment composition rules  (TODO: rename these rules)
            (doseq [r scr]
              (refine ont curr :fact (is indicatesRule (individual say-sila r))))

            ;; TODO: Prototypical code for Six Americas experimental surveys
            (doseq [s svys]
                (refine ont curr :fact (is dul/isComponentOf (Surveys s))))

            ;; Continue the reduction
            [(inc cnt)
             (conj tokens curr)])

            ;; Ignored/invalid Part of Speech tag
            (do ;(log/fmt-debug "Ignoring POS tag '~a'" tag)
                [(inc cnt) tokens])))

        [1 nil]                             ; Acc: Token counter, reverse seq of tokens
        (zip affect rules pos-tags content surveys))))))



;;; --------------------------------------------------------------------------
(defun ^OWLOntology populate-profiles
  "Populates an ontology using examples extracted from an ARFF with user data."
  ([xmps :guard map?]
  (update-kv-values xmps #(populate-profiles %1 %2)))


  ([o xmps]
  (log/debug "Populating ontology:" o)
  (let [sconf (merge (cfg/? :senti) ;;; FIXME ;;;
                     (cfg/? :sila)                     ; Cache config params
                     )              ;;; FIXME ;;;
        onter (make-ontology-maker o)]          ; Tag|ontology to factory function
    (run! (fn [{:as xmp
                sname :screen_name
                descr :description              ; User profile text
                tid   :tid}]                    ; Text ID is the profile ID
            (let [ont   (onter sname)
                  acct  (individual ont sname :type OnlineAccount)  ; Twitter user account
                  prof  (individual ont tid                         ; User profile
                                    :type PersonalProfile
                                    :fact (is dul/isAbout acct))]
              ;; Add PoS/senti for the profile content
              (add-text onter prof xmp sconf)))
          xmps)
    ;; Return the ontology maker
    onter)))



;;; --------------------------------------------------------------------------
(defn- add-dependencies
  "Incorporates a tweet's output from the TweeboParser into the specified
  ontology."
  ([onter {:keys [tid]
           :as   xmp}]
  ;(log/info "Finding dependencies for" tid)
  (add-dependencies onter xmp (twbo/predict tid)))


  ([onter
    {:keys [screen_name tid content pos-tags]
     :as   xmp}
    tweebo]
  (let [ont     (onter screen_name)
        include #(refine ont %1 :fact (is dul/hasComponent %2))
        equiv?  #(or (= %1 %2)
                     (every? #{"\"" "QUOTE"} [%1 %2]))
        make    (memoize (fn [ling n]
                            (let [tokid  (lbl/label-text-token tid n)
                                  token  (individual ont tokid)
                                  entid  (lbl/label-text-token tid n ling)
                                  entity (individual ont entid
                                           :type (case ling "CONJ"  Conjuncts
                                                            "COORD" Coordination
                                                             "MWE"  MultiWordExpression))]
                              ;; Add the relation for token-->entity to the ontology.
                              ;; The Tweebo map entry for Token N does not reference the entity.
                              ;(log/debug "Adding" ling entid)
                              (refine ont token :fact (is dul/expresses entity))

                              ;; Multi-word expression roots (n) don't have the MWE code
                              (when (= ling "MWE")
                                (include entity token))

                              ;; We will only touch the ontology once as we are memoized
                              entity)))]

    ;; Run through our example and the Tweebo parse, token by token
    (loop [[tok1                              & content*]   content     ; Tweet tokens
           [pos1                              & pos-tags*]  pos-tags    ; Parts of Speech
           [[sub tok2 _ pos2 pos3 _ obj ling] & tweebo*]    tweebo      ; Tweebo output
           deps                                             []]         ; Multi-root tree
      ;; All three arguments should be in alignment, except tweebo may have a final [""]
      (if pos1
        ;; Process the next Tweebo line
        (let [obj-num (Long/parseLong obj)
              dep?    (pos? obj-num)
              build   #(conj % (if dep? obj-num nil))]
          ;; Complain if the POS analysis doesn't match up (uncommon)
          (when (not= pos1 pos2 pos3)
              (log/fmt-warn "Part-of-speech mismatch on ~a: token[~a/~a] pos[~a~a~a]"
                          tid tok1 tok2 pos1 pos2 pos3))
          (if (equiv? tok1 tok2)
            (do
              ;; We're looking from the leaf (subject) up to the parent node (object).
              ;; -1 : subjet token is uninteresting per Tweebo
              ;;  0 : subjet token is a root node
              ;;  N : subjet token depends on the Nth token (object)
              (when dep?
                (let [[subid   objid]  (map #(lbl/label-text-token tid %) [sub obj])  ; t99-9
                      [subject object] (map #(individual ont %) [subid objid])]   ; Tokens

                ;; Add dependency relation to ontology
                ;(log/debug subid  "directlyDependsOn" objid)
                (refine ont subject :fact (is directlyDependsOn object))

                ;; Handle linguistic entities: Conjuncts, Coordinations and Multi-word expressions
                (when-let [entity (and (not= ling "_")
                                       (make ling obj))]
                  (include entity subject))))

              ;; Move on to the next token
              (recur content* pos-tags* tweebo* (build deps)))

            ;; Abort!  The parsers disagree wrt tokenization.
            (log/fmt-error "Text/tweebo mismatch on ~a: token[~a/~a] pos[~a~a~a]"
                           tid tok1 tok2 pos1 pos2 pos3)))

        ;; No more data to process. Return the dependency tree!
        (assoc xmp :deps (seq deps)))))))



;;; --------------------------------------------------------------------------
(defn- add-affirmations
  "Incorporates a tweet's output from the TweeboParser into the specified
  ontology."
  [onter
  {:keys [screen_name tid rules deps]
   :as   xmp}]

  ;(log/info "Finding negations for" tid)
  ;; Non-negated concept tokens will depend on the check we are performing here
  (let [ont (onter screen_name)                                 ; Combined|individual ontology

        ;; Create a vector with [index dep concepts] for each token
        xdeps   (into [] (cons [0 nil nil]                      ; Add an ununsed zeroth token
                               (zip (rest (range)) deps rules)))

        ;; Identify the negation and concept tokens
        {negs  true
         ctoks false} (group-by (fn [[_ _ cs]]                  ; Negation vs. [c]oncept tokens
                                  (contains? cs "NEGATION"))
                                (remove (fn [[_ _ cs]]          ; Check anything with concept(s)
                                          (empty? cs))
                                        xdeps))]

    (letfn [;; ---------------------------------------------------------------
            (affirm [[i _ _]]
              ;; Mark the token as non-negated
              ;(log/debug "Affirming token" i)
              (refine ont (individual ont (lbl/label-text-token tid i))
                          :fact (is directlyDependsOn theNotNegatedCheck)))

            ;; ---------------------------------------------------------------
            (chain
              ([[i _ _]]
                (chain i '()))

              ([i deps]
                (let [[_ d _ :as node]  (xdeps i)
                      deps*             (conj deps node)]
                  ;; Keep chaining while we have a dependency
                  (if d (recur d deps*) deps*))))

            ;; ---------------------------------------------------------------
            (negate [ctoks negs]
              ;; The last token in the chain is the negator. We don't need to process it.
              (if (empty? (next negs))
                  ctoks                         ; Remaining concept tokens are not negated
                  (recur (reduce #(disj %1 %2)  ; Remove negated concepts in chain
                                 ctoks negs)
                         (rest negs))))]        ; Ready next chain

      ;; All non-negated concept tokens must depend on a "non-negated" affirmation.
      ;; Here, negating a token implies taking it out of the set of concept tokens.
      ;(log/debug "CONCEPTS:" ctoks)
      (run! affirm (reduce negate
                           (into #{} ctoks)
                           (map chain negs))))))




;;; --------------------------------------------------------------------------
(defn populate-statuses
  "Populates the senti ontology with statuses (tweets) using examples from
  ARFFs.  The caller may specify o as a keyword, and ontology or an ontology
  factory function.  However, in all cases, the function returns an ontology
  factory function."
  ([o xmps]
  (populate-statuses o xmps (cfg/? :sila)))


  ([o xmps sconf]
  (let [onter   (make-ontology-maker o)]
    ;; Populate combined|community ontology with the tweet statuses
    (run! #(add-text onter % sconf) xmps)

    ;; Add Tweebo dependencies
    (when (get sconf :use-tweebo?)
      ;; add-dependencies will update the ontology, but we need to do a bit more processing
      (twbo/wait)
      (let [xdeps (map #(add-dependencies onter %) xmps)]

        ;; Indicate which tokens were NOT negated
        (run! #(add-affirmations onter %) xdeps)))

    onter)))


;;; --------------------------------------------------------------------------
(defn which-examples-edn
  "Returns the filepath for a user/text examples file."
  [dtag]
  (strfmt "~a/examples-~a.edn" World-FStub (name dtag)))


;;; --------------------------------------------------------------------------
(defn- create-pn-goal
  "Checks the :senti configuation and returns a map with the elements needed
  to construct datasets and populate ontologies.  The function allows the
  caller to override the configuration by adding :key value pairs as arguments."
 ([dset]
 (let [{:as   conf
        :keys [num-examples]
        :or   {num-examples senti/INIT-NUM-EXAMPLES}} (cfg/? :senti)]   ; FIXME: deprecated
    ;; The default is the number of examples for creating ontology individuals
    (create-pn-goal dset num-examples conf)))


 ([dset cnt]
 (create-pn-goal dset cnt (cfg/? :senti)))


 ([dset cnt {:as   conf
             :keys [balance?]}]
 ;; Unless it's a singleton, odd counts that are balanced will have an extra instance
 (let [[goal
        checks] (if (and balance?
                         (> cnt 1))
                    [(int (/ cnt 2)) [:positive :negative]]     ; pos/neg instances separately
                    [cnt [dset]])]                              ; all instances together

   ;; Add what we need for our goals
   (assoc conf :goal    goal
               :checks  checks
               :dataset dset))))


;;; --------------------------------------------------------------------------
(defn- split-pn-goals
  "Returns a map of pos/neg creation goals for creating :train and :test
  datasets."
  [dset]
  (let [{:as   conf
         :keys [data-split]} (cfg/? :senti)]

    (into {} (map #(vector % (create-pn-goal dset (% data-split) conf))
                  Split-Tags))))



;;; --------------------------------------------------------------------------
(defn- describe-creation
  "Returns a string describing the creation goals."
  ([{:keys [balance?
            dataset
            extra-info
            goal]}]
   (str (if balance? (str "Balancing " goal "/" goal)
                     (str "Creating "  goal))
        " " (name dataset)
        (when extra-info
          (str " " extra-info))))


  ([goals tt]
  (describe-creation (assoc (goals tt)
                            :extra-info (name tt)))))


;;; --------------------------------------------------------------------------
(defn- creation-done?
  "Returns true if the callers creation activites have completed."
  [cnts
   {:keys [goal checks]}]
  (every? #(>= (cnts %) goal) checks))



;;; --------------------------------------------------------------------------
(defn- creation-full?
  "Returns true if a (pos|neg balanced) category has filled up during  the
  creation of a dataset or an example set."
  [cnts pole
   {:keys [balance? goal checks]}]
  (and (not= pole :?)                       ; Not under evaluation
       balance?
       (>= (cnts pole) goal)))



;;; --------------------------------------------------------------------------
(defn- zero-pn-counter
  "Returns a map used to initialize counting SCR examples or data instances."
  [dset]
  (reduce #(assoc %1 %2 0) {} [dset :positive :negative :?]))



;;; --------------------------------------------------------------------------
(defn- inc-pn-counter
  "Returns an updated map after incrementing the couter values for the dataset
  and the specified polarity."
  [cnts dset pn]
  (update-values cnts [dset pn] inc))



;;; --------------------------------------------------------------------------
(defrecord Toolbox [all-pn?         ; Use only texts with sentiment/emotion
                    stoic?          ; Determines if text is void of sentiment
                    stem            ; Find grammatical stem for a word
                    sense           ; Identify tokens with sentiment/emotion
                    scr             ; Identify tokens invoking sentiment composition rules
                    surveys])       ; Identify surveys where this word is a keyword


(defn toolbox
  "Creates and bundles utility functions used for processing textual examples
  with respect to sentiment/emotion content and sentiment composition rules.
  This function bundle is tuned by parameters in the :senti section of the
  configuration."
  []
  ;; Create a closure for a configuration-based analysis
  (let [all-pn? (cfg/?? :sila :skip-neutrals?)
        lex     (tw/make-lexicon (cfg/?? :sila :lexicon :liu))  ; TODO: Capture lex change on config update
        sball   (tw/make-stemmer)                               ; Weka Affective Tweets plus Snowball stemmer
        stem    (fn [w]
                  (.stem sball w))

        exprs   (update-values Expressions                      ; Pre-stem Liu's SCR expressions
                               #(into #{} (map stem %)))]

    ;; Bundle everything up
    (map->Toolbox
     {:all-pn?  (fn [] all-pn?)

      :stoic?   (fn [{:keys [analysis]}]                        ; Check that we're not including neutral Texts
                  (and all-pn? (every? empty? analysis)))       ; ..and that no sentiment (rule) is expressed

      :stem     stem

      :sense    #(tw/analyze-token+- lex % Affect-Fragments)    ; Lexicon lookup for P/N rules

      :scr      #(let [term (stem %)]                           ; Match terms for Sentiment Composite Rules
                   (reduce (fn [acc [scr terms]]
                             (if (contains? terms term)
                                 (conj acc scr)
                                 acc))
                           #{}
                           exprs))

      :surveys  #(reduce (fn [acc s]                            ; Link Six Americas surveys
                           (if (six/in-survey? s % sball)
                               (conj acc s)
                               acc))
                          #{}
                          (keys Surveys))})))



;;; --------------------------------------------------------------------------
(defn make-example
  "Processes raw data to create a hashmap representing an instance in
  'example' form, which is an intermediate structure in a conversion to
  a textual individual in an ontology."
  ([tools tid sname elements]
  (make-example tools tid sname elements :?))


  ([tools tid sname elements polarity]
  (let [pairs   (map #(str/split % #"_" 2)                      ; Separate elements: [PoS token]
                      (str/split elements #" "))

        terms   (map #(-> % (second)                            ; FIXME: Get terms using
                            (str/lower-case)                    ;  affective.core.Utils/tokenize
                            (.replaceAll "([a-z])\\1+" "$1$1")) ;  repeated letters
                      pairs)

        affect  (map (:sense tools) terms)           ; Affect: pos|neg|emo or nil per term
        rules   (map (:scr tools) terms)]            ; Set of match-term rules per term

    ;; Put all that together to build the example
    {:screen_name sname
     :tid         tid
     :polarity    polarity
     :content     terms
     :rules       rules
     :pos-tags    (map first pairs)
     :surveys     (map (:surveys tools) terms)
     :analysis    (map set/union affect rules)})))



;;; --------------------------------------------------------------------------
(defn profiles->examples
  "Converts Weka user (U99) instances into corresponding examples in the
  intermediate format."
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
             (let [avals (update-values attrs #(.stringValue inst (int %)))     ; Pull attr-vals
                   sname (:screen_name avals)
                   tid   (str "ProfileOf_" sname)                               ; TextID is profile name
                   xmp   (make-example tools tid sname (:description avals))]   ; Check emotion
             ;; Add on hashmap with attribute data plus emotion analysis
             (conj acc (merge avals xmp))))
         '()
         (weka/instance-seq insts)))))


  ([dset data]
  (hash-map dset (profiles->examples data))))



;;; --------------------------------------------------------------------------
(defn statuses->examples
  "Converts Weka status (S99) instances (tweets) into a sequence of examples
  in the intermediate format."
  ([dset data]
  (let [insts (weka/load-dataset data (senti/which-target))     ; FIXME: deprecated
        icnt  (.numInstances insts)]
    (log/fmt-debug "Text instances~a: ~a" dset icnt)
    (senti/instances->examples dset insts icnt)))               ; FIXME: deprecated


  ([dset ^Instances insts cnt]
  ;; Keep track of how many examples to create, as per the configured 'balance' setting
  (let [columns     (dset/columns :s)
        [col-id
         col-sname
         col-text]  (map columns [:id :screen_name :text])
        tools       (toolbox)
        stoic?      (:stoic? tools)
        goal        (create-pn-goal dset cnt)]

    ;; The number of examples we're creating depends on how things were configured
    (log/info (describe-creation goal)
              "SCR examples [pos/neg]"
              (if ((:all-pn? tools)) "(emotive)" "(includes stoic)"))

    ;; Shall we (pseudo)randomize the instances?
    (when-let [seed (cfg/?? :senti :rand-seed)]
      (log/fmt-info "Shuffling ~a input instances: seed[~a]" (.numInstances insts) seed)
      (.randomize insts (Random. seed)))

    ;; Throw away the counter & return the folded example sequence
    (second
      (reduce (fn [[cnts xmap :as info]                             ; FUN: add a textual eXample
                   ^Instance inst]
                ;(log/debug "Counts:" cnts)
                ;; Do we have enough examples to stop?
                (if (creation-done? cnts goal)
                  (do (log/info "Examples:" cnts)
                      (reduced info))
                  (let [tid    (lbl/label-text (.stringValue inst (int col-id)))
                        sname  (.stringValue inst (int col-sname))
                        pole   (lbl/polarize inst)
                        elms   (.stringValue inst (int col-text))       ; Text elements are "pos_term"
                        xmp    (make-example tools tid sname elms pole) ; Example as a hashmap
                        xkeys  (apply set/union #{dset} (xmp :rules))]  ; Full dataset & all SCRs
                    ;; Do we skip|process this Text??
                    (if (or (stoic? xmp)                                ; Is it void of pos/neg/emotion?
                            (creation-full? cnts pole goal))            ; Still collecting for this polarity?
                      info
                      [(inc-pn-counter cnts dset pole)                  ; Update pos/neg/all counts
                       (update-values xmap xkeys #(conj % xmp))]))))

            [(zero-pn-counter dset)                                 ; ACC: total/pos/neg counts
             (reduce #(assoc %1 %2 #{}) {} (keys Expressions))]     ;      Examples keyed by rule

            (enumeration-seq (.enumerateInstances insts)))))))      ; SEQ: Weka instances



;;; --------------------------------------------------------------------------
(defn create-world!
  "Loads the official say-sila examples and ontologies."
  ([]
  ;; Pull the configured dataset information for users and their texts.
  ;; TODO: Incorporate dset/t->su
  (let [{dtag   :tag
         track  :tracker
         src    :source
         dir    :dir}   (cfg/?? :sila :data Init-Data)

        ;; Pull [a]rff users and texts
        [ausers atexts] (map #(apply strfmt "~a/~a.~a.~a.~a.arff"
                                            (map name [dir src track dtag (dset/code %)]))
                              [:user :senti])]
    ;; Now we've got ARFF datasets, load % process them
    (create-world! dtag ausers atexts)))


  ([dtag]
  ;; Reuse a saved set of examples (rather than constructing news ones per the configuration)
  (let [fpath (which-examples-edn dtag)]
    (when (.exists (io/file fpath))
      (log/fmt-info "User/text examples~a: ~a" dtag fpath)
      (create-world! dtag (edn/read-string (slurp fpath))))))


  ([dtag {:keys [users texts]
          :as   xmps}]
  ;; This middle arity clause is where we wrap everything up!
  ;; Make an ontology out of the passed e[x]ample hashmap.
  (let [sconf  (cfg/? :sila {})
        world  (-> (populate-profiles dtag (users dtag))
                   (populate-statuses (texts dtag) sconf))]

    ;; Set our top-level state. Each element holds a tagged map of the appropriate data
    (reset! World (assoc xmps :ontology {dtag world}))
    (send Memory conj [:world (jvm/memory-used :MB)])
    dtag))


  ([dtag ausers atexts]
  ;; Create e[x]amples from the source [a]arff files
  (let [xusers (profiles->examples dtag ausers)             ; U-dataset user profiles
        xtexts (statuses->examples dtag atexts)]            ; S-dataset tweet texts

    ;; Now we've got ARFF datasets, load % process them
    (log/fmt-info "Dataset user~a: ~a" dtag ausers)
    (log/fmt-info "Dataset text~a: ~a" dtag atexts)

    (create-world! dtag {:users xusers
                         :texts xtexts}))))



;;; --------------------------------------------------------------------------
(defn report-examples
  "Give positive/negative coverage and sentiment statistics for sets of
  intermediate-format examples or a hashmap with multiple sets of examples
  as values."
  ([xmps]
  (if (map? xmps)
      (run! #(apply report-examples %)                          ; Report keyed example sets
            (select-keys xmps (filter keyword? (keys xmps))))   ; ..ignoring s"CONCEPT" submaps
      (report-examples :examples xmps)))                        ; Single set of examples


  ([dtag xmps]
  (let [;; Statistics on text polarity and presence of affect
        stats   (reduce #(let [ss (if (every? empty? (:affect %2))
                                      :stoic
                                      :senti)]
                           (update-values %1 [:count (:polarity %2) ss] inc))
                        (zero-hashmap :count :positive :negative :? :senti :stoic)
                        xmps)

        ;; Generalized roll-up functionality
        p100    #(* 100. (/ (stats %)
                            (stats :count)))

        zero    #(apply zero-hashmap %)
        init    #(vector (map %1 xmps)                          ; [elements, initial count-map]
                         (zero %2))
        kount   #(update-values %1 %2 inc)                      ; Accumulate hits from seq %2

        count-tokens    (fn [zeros elements]
                          (reduce kount
                                  zeros
                                  (flatten (map #(remove empty? %) elements))))

        count-texts     (fn [zeros elements]
                          (reduce #(update-values %1 (apply set/union %2) inc)
                                  zeros
                                  elements))

        ;; We'll need a sequence of affect (rule) sets for the Texts
        [aff-rules                                              ; Affect sets from Texts
         aff-zeros] (init :affect Affect-Names)                 ; Acc init: affect counts

        aff-toks    (count-tokens aff-zeros aff-rules)
        aff-texts   (count-texts  aff-zeros aff-rules)

        ;; Six Americas surveys
        [svy-hits                                               ; Keyword hits from surveys
         svy-zeros] (init :surveys (keys Surveys))              ; Acc init: survey counts

        svy-toks    (count-tokens svy-zeros svy-hits)
        svy-texts   (count-texts  svy-zeros svy-hits)

        ;; Now get a sequence of part-of-speech tags for the Texts.
        [pos-tags                                               ; POS tags for all Texts
         pos-zeros] (init :pos-tags pos/POS-Codes)              ; Acc init: POS tag counts

        pos-toks  (reduce kount pos-zeros pos-tags)
        pos-texts (reduce (fn [cnts pos]
                            (update-values cnts (into #{} pos) inc))
                          pos-zeros
                          pos-tags)]

  ;; Report the basic statistics
  (log/fmt-info "SCR~a: p[~1$%] s[~1$%] xmps~a"
                dtag (p100 :positive) (p100 :senti) stats)

  ;; Report pos/neg first, then the emotions
  (doseq [aff (conj (sort (keys (dissoc aff-texts "Positive" "Negative")))  ; ABCize emotions
                    "Negative"                                              ; Add onto head
                    "Positive")]                                            ; ..of the list
    (log/fmt-debug "Affect~a ~12a [~4d Tokens in ~4d Texts]"
                   dtag aff
                   (get aff-toks  aff)
                   (get aff-texts aff)))

  ;; Six Americas surveys
  (log/debug)
  (doseq [svy (sort (keys svy-texts))]
    (log/fmt-debug "Survey~a ~12a [~4d Tokens in ~4d Texts]"
                    dtag (name svy)
                    (get svy-toks svy)
                    (get svy-texts svy)))

  ;; Report part-of-speech tags
  (log/debug)
  (doseq [pos (sort-by pos/POS-Fragments
                      (keys pos-texts))]
    (log/fmt-debug "Speech~a ~24a [~4d Tokens in ~4d Texts]"
                   dtag
                   (pos/POS-Fragments pos)
                   (get pos-toks  pos)
                   (get pos-texts pos))))))


;;; --------------------------------------------------------------------------
(defn report-accounts
  "Gives instance coverage of specific online account classes."
  ([]
  (report-accounts @World))


  ([world]
  ;; NOTE: The newer community way is subtly different from the original say-sila world
  (if (cfg/?? :sila :community?)
    ;; The community approach currently supports (just) a single set of ontologies
    (report-accounts (first (keys (:ontology world)))       ; The single data tag
                     (comm/fetch))
    ;; The base say-sila approach has multiple (big) ontologies, keyed by data tags
    (run! (fn [[dtag onter]]
            (report-accounts dtag [(onter)]))
            (:ontology world))))


  ([dtag onts]
  (let [targets '[HumanCauseBelieverAccount
                  NaturalCauseBelieverAccount]

        search  (fn [ont]
                  ;; Find all instances for the search classes
                  (let [hits (reduce #(conj %1 [%2 (rsn/instances ont (eval %2))])
                                     {}
                                     targets)]
                    ;; Reclaim memory from reasoner
                    (inf/unreason ont)
                    hits))

        needles (inf/with-silence
                  (reduce #(merge-with set/union %1 %2) {} (pmap search onts)))

        report  (fn [sym]
                  (let [accts (get needles sym)]
                    (log/info (str sym dtag ":") (count accts))
                    (comment run! #(log/debug "  -" (iri-fragment %)) accts)))]

    ;; Log report to the console for all targets
    (run! report targets))))



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
            (report-examples (world elm))
            (log/debug))
          [[:users "User Profiles:"]
           [:texts "User Tweets:"]])

    (doseq [[tag us] users]
      (log/debug)
      (log/fmt-info "USERS~a: ~a" tag us))))



;;; --------------------------------------------------------------------------
(defn eprint-user
  "Pretty-prints a user's profile and tweets, highlighting the affect and
  showing a token dependency tree if available."
  [user]
  ;; NOTE: we may want to redo our keying system so the data tag is the top level
  (let [eprint (fn [xmps]
                 (run! #(senti/eprint-tweet %)
                       (filter #(= user (:screen_name %)) xmps)))]
    ;; Run through the supported text-types
    (run! (fn [ttype]
            ;; Group profile/tweet reports by the dataset
            (run! (fn [[dtag elms]]
                    (log/fmt! "~a'~a' ~a ~a~a\n" log/Bright
                                                 (name dtag)
                                                 (case ttype :users "Profile of"
                                                             :texts "Tweets for")
                                                 user
                                                 log/Text)
                    (eprint elms))
                  (@World ttype)))
          [:users :texts])))



;;; --------------------------------------------------------------------------
(defn save-ontologies
  "Saves the say-sila ontology and all World ontologies in OWL format.

  FIXME: save-ontology-map is expecting ontologies, but we now have
         ontology-maker functions.  We're leaving this broken until we
         have a working onto-community architecture.  Until then, use

            (save-ontology ((-> @World :ontology :cause))

         in the REPL."
  ([]
  (save-ontologies @World))


  ([world]
  (save-ontology say-sila Ont-FPath :owl)
  (merge {:say-sila Ont-FPath}
         (save-ontology-map (:ontology world) Ont-FStub))))



;;; --------------------------------------------------------------------------
(defn save-world
  "Saves the World example information.  Note that you must specify the :ont
  option to also save the associated ontologies."
  [& opts]
  (let [world @World
        wtag  (apply hyphenize (keys (:users world)))   ; Multi-dtag support
        fpath (which-examples-edn wtag)]

    (spit fpath (pr-str (select-keys world [:users :texts])))

    ;; Return a map of the files saved
    (merge {:examples fpath}
           (when (some #{:ont} opts)
             (save-ontologies world)))))



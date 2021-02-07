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
            [say.social         :as soc]
            [say.survey         :as six]
            [say.tweebo         :as twbo]
            [say.wordnet        :as word]
            [weka.core          :as weka]
            [weka.dataset       :as dset]
            [weka.tweet         :as tw]
            [clojure.edn        :as edn]
            [clojure.java.io    :as io]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp]]
            [defun.core         :refer [defun]]
            [incanter.core      :refer [dataset $data view with-data]]
            [incanter.charts    :refer [stacked-bar-chart set-stroke-color]]
            [me.raynes.fs       :as fs]
            [tawny.english      :as dl]
            [tawny.reasoner     :as rsn]
            [tawny.query        :as qry]
            [tawny.repl         :as repl]                   ; <= debug
            [tawny.owl          :refer :all]
            [clojure.core.logic :refer :all :exclude [annotate is run]])
  (:import  (java.awt Color)
            (java.util Random)
            (org.jfree.chart JFreeChart)
            (org.jfree.chart.renderer.category StackedBarRenderer)
            (org.semanticweb.owlapi.model   IRI
                                            OWLOntology
                                            OWLOntologyID)
            (weka.core Attribute
                       DenseInstance
                       Instance
                       Instances)))


;;; --------------------------------------------------------------------------
;;; TODO:
;;; - defworldfn macro
;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Ont-IStub      "http://www.dendrown.net/uqam/say-sila")
(def ^:const Ont-IRI        "http://www.dendrown.net/uqam/say-sila.owl#")
(def ^:const Ont-FPath      "resources/KB/say-sila.owl")
(def ^:const Ont-FStub      "resources/KB/say-sila")
(def ^:const Emotion-FStub  "resources/world")
(def ^:const World-FStub    "resources/world")
(def ^:const Data-Plan-Dir  "resources/data-plan")
(def ^:const Tmp-Dir        "/tmp/say_sila")                ; Shared with Erlang

(def ^:const Init-Data      {:tag :env, :tracker :all, :source :tweets, :dir Emotion-FStub})


;;; --------------------------------------------------------------------------
(defonce Memory         (agent {:start (jvm/memory-used :MB)})) ; Memory used in megabytes

(defonce Base-Plutchik '[Anger Fear ,, Sadness Joy        ,, Surprise Anticipation ,, Disgust Trust])
(defonce Base-Ekman    '[Anger Fear    Sadness Happiness     Surprise                 Disgust])

;;; Word sets which invoke Survey Concept Rules
(defonce Rule-Triggers  (merge six/Concept-Triggers
                               {"NEGATION"     #{"not"}}))          ; Syntactical adjusters

(defonce Rule-Words     (word/synonym-values Rule-Triggers))        ; Rule trigger expansion
(defonce Rule-Stems     (update-values Rule-Words
                                       #(tw/stem-all % :set)))

(defonce World          (atom {:users {}
                               :texts {}
                               :ontology {}
                               :community {}}))

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
(as-disjoint Text pos/Token)

;;; Keep the actual tweet/profile content as a development aid
(defaproperty TextualContent)

(defclass OnlineAccount
  :super    dul/SocialObject
  :label    "Online Account"
  :comment  "A user account for an online service.")

;;; Help DL-Learner to not confuse our primary entities
(as-disjoint OnlineAccount dul/InformationObject dul/Quality)


(as-inverse
  (defoproperty publishes
    :label    "publishes"
    :domain   OnlineAccount
    :range    dul/InformationObject
    :comment  "The action of making an Information Object available to an online community.")

  (defoproperty isPublishedBy
    :label    "is published by"
    :domain   dul/InformationObject
    :range    OnlineAccount
    :comment  (str "Indicates that the Information Object has been made available "
                   "to an online community by the Online Account.")))


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
(as-disjoint Text Survey)

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
              :plutchik (map emote Base-Plutchik)
              :ekman    (map emote Base-Ekman)
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


(defmacro affectize
  "Runs the action function/macro for all defined affect elements."
  [action]
  `(do ~@(for [aff# Affect-Names]
           `(~action ~aff#))))


;;; --------------------------------------------------------------------------
;;; Environmental clues at the Text level
;;;
;;; TBox: building on pos:Token
(defmacro def-affect-token
  "Creates an affect Token class for the given (String) sentiment polarity or emotion."
  [aff]
  (let [sym (eval `(as-symbol ~aff "Token"))]
    `(defclass ~sym
       :super    pos/Token
       :label    (str ~aff " Token")
       :comment  (str "A Token which may indicate " ~aff ".")
       :equivalent (dl/and pos/Token
                           (dl/some denotesAffect (owl-class say-sila ~aff))))))

;;; Create affect Token and Information Objects for all defined polarities and emotions
(affectize def-affect-token)                        ; Define AngerToken, etc...


(defmacro def-affect-info-obj
  "Creates an affect InformationObject class for the given (String) sentiment polarity or emotion."
  [aff]
  `(defclass ~(symbol (str aff "InformationObject"))
    :super    dul/InformationObject
    :label    (str ~aff " Information Object")
    :comment  (str "An Information Object which may indicate " ~aff ".")
    :equivalent (dl/and dul/InformationObject
                        (dl/some dul/hasComponent ~(symbol (str aff "Token"))))))

;; NOTE: There is an issue with DL-Learner never returning when it has too many
;;       [Affect][PoS]InformationObjects to play with.
(affectize def-affect-info-obj)                     ; Define AngerInformationObject, ...


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
           `(do (def-affect-pos-token ~aff ~pos)
                ;; FIXME: Looking into issues with DL-Learner never returning when it has too many
                ;;        [Affect][PoS]InformationObjects to play with.
                (comment def-affect-pos-info-obj ~aff ~pos)))))

(def-affect-pos-classes ["CommonNoun" "Verb"])


;;; --------------------------------------------------------------------------
(defclass SurveyConceptRule
  :super   dul/Concept
  :label   "Survey Concept Rule"
  :comment (str "An abstraction describing a Text or portion of a Text, "
                 "indicating that it refers to a question from a Six Americas survey."))


;;; Are we using specialized object properties?
(defoproperty indicatesRule
  :super   dul/expresses
  :label   "indicates rule"
  :domain  pos/Token
  :range   SurveyConceptRule
  :comment "A relationship between a Token and the survey reference rule it expresses.")


(defn rule-symbol
  "Create symbols based on the standard that rule concepts are in all-caps,
  and related concepts are expressed in PascalCase."
  [& parts]
  (let [[rules
         entity] (butlast-last parts)
        recase   #(if (contains? #{"CO2"} (str %))
                      %
                      (str/capitalize %))]
    (symbol (apply str (conj (mapv recase rules) entity)))))


(defmacro defrule
  "Adds a Sentiment Composition Rule subclass (the conceptual component)
  to the say-sila ontology"
  [concept descr]
  (let [rule `~concept]                     ; Ensure single evaluation
    `(do (def ~rule)
         (if (bound? #'~rule)
             (log/debug "Using existing definition of" '~rule)
             (do (defclass ~rule
                   :super   SurveyConceptRule
                   :label   (str "Survey Concept Rule - " '~rule)
                   :comment ~descr)
                 (defpun ~rule))))))


;;; Concept indictor rules
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

(comment defclass SurveyConcept     ; TODO: Pending decision on info-objs
  :super    dul/InformationObject
  :label    "Survey Concept"
  :comment  (str "An Information Object which has one or more keywords from a "
                 "Six Americas survey.")
  :equivalent (dl/and dul/InformationObject
                      (dl/some dul/hasComponent SurveyKeyword)))


(comment defclass BeliefsQuestionKeyword    ; TODO: Tie questions in with keyword concepts
  :super    SurveyKeyword
  :label    "Beliefs Question Keyword"
  :comment  "A Keyword which is refers to the question on beliefs (Table 5) in the Six America's survey.")


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


;;; TBox: building on dul:InformationObject==>sila:Text
(defclass PersonalProfile
  :super   Text
  :label   "Personal Profile"
  :comment "An Information Object consisting of a personal description for an online user.")

(comment
(defclass Tweet
  :super   Text
  :label   "Tweet"
  :comment "A Twitter message status message.")
(as-disjoint PersonalProfile Tweet)

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
  :comment  "An Online Account that represents someone who does not believe in anthropogenic climate change.")

(defclass GreenAccount
  :super    OnlineAccount
  :label    "Green Account"
  :comment  "An Online Account that represents someone who is concerned about the environment.")

(defclass RogueAccount
  :super    OnlineAccount
  :label    "Rogue Account"
  :comment  "An Online Account which does not adhere to the rules of its associated online provider.")

;;; --------------------------------------------------------------------------
(defmacro defscr-token
  "Defines a class representing a Token that implies a Survey Concept Rule."
  [concept]
  (let [rule  `~concept
        token (rule-symbol rule "Token")]
    `(do (def ~token)
         (if (bound? #'~token)
             (log/debug "Using existing definition of" '~token)
             (defclass ~token
               :super pos/Token
               :label (soc/tokenize '~token :str)
               :equivalent (dl/and pos/Token
                                   (dl/some indicatesRule ~rule)))))))



(defmacro defscr-token-dep
  "Defines a class representing one Token that is dependent on another."
  [dep-token token1 token2]
  `(defclass ~dep-token
     :super pos/Token
     :label (soc/tokenize '~dep-token :str)
     :equivalent (dl/and ~token1
                         (dl/some dependsOn ~token2))))


(defmacro defscr-text-dep
  "Defines a class representing a Text that contains the specified Token
  dependency relation."
  [dep-text dep-token rule1 rule2]
  `(defclass ~dep-text
     :super Text
     :label (soc/tokenize '~dep-text :str)
     :comment (str "A Text with components representing the concepts of " '~rule1 " and " '~rule2
                   "where the " '~rule1 " Token depends syntactically on " '~rule2)
     :equivalent (dl/and Text
                         (dl/some dul/hasComponent ~dep-token))))


(defmacro defscr-accounts
  "Defines a class representing an Online Account has published a text of the
  specifed type."
  [acct text]
  (let [account `~acct
        green   (as-symbol "Green"  account)
        denier  (as-symbol "Denier" account)
        label   (soc/tokenize account :str)]
    `(do
      (defclass ~account
        :super OnlineAccount
        :label ~label
        :equivalent (dl/and OnlineAccount
                            (dl/some publishes ~text)))
      (as-disjoint
       (defclass ~green
         :super GreenAccount
         :label (str "Green " ~label)
         :equivalent (dl/and GreenAccount ~account))

       (defclass ~denier
         :super DenierAccount
         :label (str "Denier " ~label)
         :equivalent (dl/and DenierAccount ~account))))))


(defmacro defscr-2
  "Defines all necessary elements for a two-concept survey concept rule."
  [concept1 descr1
   concept2 descr2]
  ;; We need all the symbols ready for insertion in the quasiquote code.
  ;; NOTE:  and : token1 & token2 are both present (but not necessarily dependent)
  ;;        dep : token1 depends on token2
  (let [;; We should be working with raw symbols, but ensure single evaluation
        rule1       `~concept1
        rule2       `~concept2
        token1      (rule-symbol rule1 "Token")
        token2      (rule-symbol rule2 "Token")
        token1->2   (rule-symbol rule1 rule2 "TokenAB")
        token2->1   (rule-symbol rule1 rule2 "TokenBA")

        text1++2    (rule-symbol "Weak"   rule1 rule2 "Text")
        text1<>2    (rule-symbol "Strong" rule1 rule2 "Text")
        text1->2    (rule-symbol "Strong" rule1 rule2 "TextAB")
        text2->1    (rule-symbol "Strong" rule1 rule2 "TextBA")

        account1++2 (rule-symbol "Weak"   rule1 rule2 "Account")
        account1<>2 (rule-symbol "Strong" rule1 rule2 "Account")
        account1->2 (rule-symbol "Strong" rule1 rule2 "AccountAB")
        account2->1 (rule-symbol "Strong" rule1 rule2 "AccountBA")]
  `(do
    ;; Basic Survey Concept Rule indicators
    (defrule ~rule1 ~descr1)
    (defrule ~rule2 ~descr2)

    ;; Token definitions
    (defscr-token ~rule1)
    (defscr-token ~rule2)
    (defscr-token-dep ~token1->2 ~token1 ~token2)
    (defscr-token-dep ~token2->1 ~token2 ~token1)

    ;; Define Text containing the Tokens various relations
    (defclass ~text1++2
      :super Text
      :label (soc/tokenize '~text1++2 :str)
      :comment (str "A Text with components representing concepts of " ~rule1 " and " ~rule2)
      :equivalent (dl/and Text
                          (dl/some dul/hasComponent ~token1)
                          (dl/some dul/hasComponent ~token2)))

    (defscr-text-dep ~text1->2 ~token1->2 ~rule1 ~rule2)
    (defscr-text-dep ~text2->1 ~token2->1 ~rule2 ~rule1)
    (defclass ~text1<>2
      :super Text
      :label (soc/tokenize '~text1<>2 :str)
      :comment (str "A Text with components representing the concepts of " ~rule1 " and " ~rule2
                    "where these concepts are in arelationship of syntactical dependency")
      :equivalent (dl/and Text
                          (dl/some dul/hasComponent (dl/or ~token1->2
                                                           ~token2->1))))

    ;; User accounts publishing Texts with these Tokens
    (defscr-accounts ~account1++2 ~text1++2)
    (defscr-accounts ~account1<>2 ~text1<>2)
    (defscr-accounts ~account1->2 ~text1->2)
    (defscr-accounts ~account2->1 ~text2->1))))

;; Accounts identified by Survey Concept Rules
;;
;; ├── is (V)
;; :   ├── why (R)
;;     └── saving (V)
;;         ├── energy (N)
;;         └── ++important++ (A)
(defscr-2 ENERGY "Expressions which refer to energy"
          CONSERVATION "Expressions which indicate a relationship of convervation")

;; ├── reduces (V)
;; :  └── levels (N)
;;         └── co2 (^)
(defscr-2 CO2 "Expressions which indicate a relationship of convervation"
          CUT "Expressions which refer to reductions")

;; └── ++protect++ (V)
;; :   ├── environment (N)
;;     :   └── the (D)
(defscr-2 ENVIRONMENT "Expressions which refer to the environment"
          PROTECT     "Expressions which indicate a relationship of protection")

;; ├── advancing (V)
;; :   └── ++growth++ (N)
;;         └── economic (A)
(defscr-2 ECONOMIC "Expressions which refer to the economy"
          GROWTH   "Expressions which indicate a relationship of growth")

(defscr-2 HUMAN  "Expressions which refer to humans or humanity."
          CAUSE  "Expressions which indicate a causal relationship.")

(defscr-2 NATURE "Expressions which refer to the natural world."
          CAUSE  "Expressions which indicate a causal relationship.")

(defscr-2 PEOPLE "Expressions which indicate a group of persons."
          HARM   "Expressions which refer to hurt or harm.")

(defscr-2 COMPANY "Expressions which indicate..."                   ; TODO: remove (low coverage)
          REWARD  "Expressions which refer to...")

(defscr-2 COMPANY "Expressions which indicate..."                   ; TODO: remove (low coverage)
          PUNISH  "Expressions which refer to...")

;; Accout1 uses SCRs we expect to be green
(defclass WeakInferredGreenAccount1
  :super    OnlineAccount
  :label    "Weak Inferred Green Account (type 1)"
  :equivalent (dl/and OnlineAccount
                      (dl/or WeakHumanCauseAccount
                             WeakEnergyConservationAccount
                             WeakCO2CutAccount
                             WeakEnvironmentProtectAccount)))

(defclass StrongInferredGreenAccount1
  :super    OnlineAccount
  :label    "Strong Inferred Green Account (type 1)"
  :equivalent (dl/and OnlineAccount
                      (dl/or StrongHumanCauseAccount
                             StrongEnergyConservationAccount
                             StrongCO2CutAccount
                             StrongEnvironmentProtectAccount)))


(defclass GreenWeakInferredGreenAccount1
  :super    GreenAccount
  :label    "Green Weak Inferred Green Account (type 1)"
  :equivalent (dl/and GreenAccount
                      WeakInferredGreenAccount1))

(defclass GreenStrongInferredGreenAccount1
  :super    GreenAccount
  :label    "Green Strong Inferred Green Account (type 1)"
  :equivalent (dl/and GreenAccount
                      StrongInferredGreenAccount1))


(defclass DenierWeakInferredGreenAccount1
  :super    DenierAccount
  :label    "Denier Weak Inferred Green Account (type 1)"
  :equivalent (dl/and DenierAccount
                      WeakInferredGreenAccount1))

(defclass DenierStrongInferredGreenAccount1
  :super    DenierAccount
  :label    "Denier Strong Inferred Green Account (type 1)"
  :equivalent (dl/and DenierAccount
                      StrongInferredGreenAccount1))


;; Accout2 uses additional SCRs that represent common denier talking points
(defclass WeakInferredGreenAccount2
  :super    OnlineAccount
  :label    "Weak Inferred Green Account (type 2)"
  :equivalent (dl/and OnlineAccount
                      (dl/or WeakHumanCauseAccount
                             WeakEnergyConservationAccount
                             WeakCO2CutAccount
                             WeakEnvironmentProtectAccount
                             ; Talking about traditional denier stances
                             WeakNatureCauseAccount
                             WeakEconomicGrowthAccount)))

(defclass StrongInferredGreenAccount2
  :super    OnlineAccount
  :label    "Strong Inferred Green Account (type 2)"
  :equivalent (dl/and OnlineAccount
                      (dl/or StrongHumanCauseAccount
                             StrongEnergyConservationAccount
                             StrongCO2CutAccount
                             StrongEnvironmentProtectAccount
                             ; Talking about traditional denier stances
                             StrongNatureCauseAccount
                             StrongEconomicGrowthAccount)))

(defclass GreenWeakInferredGreenAccount2
  :super    GreenAccount
  :label    "Green Weak Inferred Green Account (type 2)"
  :equivalent (dl/and GreenAccount
                      WeakInferredGreenAccount2))

(defclass GreenStrongInferredGreenAccount2
  :super    GreenAccount
  :label    "Green Strong Inferred Green Account (type 2)"
  :equivalent (dl/and GreenAccount
                      StrongInferredGreenAccount2))


(defclass DenierWeakInferredGreenAccount2
  :super    DenierAccount
  :label    "Denier Weak Inferred Green Account (type 2)"
  :equivalent (dl/and DenierAccount
                      WeakInferredGreenAccount2))

(defclass DenierStrongInferredGreenAccount2
  :super    DenierAccount
  :label    "Denier Strong Inferred Green Account (type 2)"
  :equivalent (dl/and DenierAccount
                      StrongInferredGreenAccount2))


;;; --------------------------------------------------------------------------
;;; Original (pre-scr) combinations for analysis:
;;; FIXME: Make a decision about the AFFIRM-NEGATE strategy
;;;        so that we may remove the stragglers!
(when (cfg/?? :sila :use-tweebo?)

  (defclass HumanCauseTokenAFFNEG
    :super pos/Token
    :equivalent (dl/and pos/Token
                        (dl/or
                          (dl/and
                            (dl/some indicatesRule HUMAN)
                            (dl/some indicatesRule CAUSE))
                          (dl/and
                            (dl/some indicatesRule HUMAN)
                            (dl/some dependsOn (dl/some indicatesRule CAUSE))))))

  (defclass NaturalCauseTokenAFFNEG
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
    (defclass NegatedHumanCauseTokenAFFNEG
      :super HumanCauseTokenAFFNEG
      :equivalent (dl/and HumanCauseTokenAFFNEG
                          (dl/some hasDependent NegationToken)))

    (defclass AffirmedHumanCauseTokenAFFNEG
      :super HumanCauseTokenAFFNEG
      :equivalent (dl/and HumanCauseTokenAFFNEG
                          (dl/some directlyDependsOn NotNegatedCheck))))

  ;; Natural-cause indicators follow the same pattern
  (as-disjoint
    (defclass NegatedNaturalCauseTokenAFFNEG
      :super NaturalCauseTokenAFFNEG
      :equivalent (dl/and NaturalCauseTokenAFFNEG
                          (dl/some hasDependent NegationToken)))

    (defclass AffirmedNaturalCauseTokenAFFNEG
      :super NaturalCauseTokenAFFNEG
      :equivalent (dl/and NaturalCauseTokenAFFNEG
                          (dl/some directlyDependsOn NotNegatedCheck))))

  ;; Text-level causality (currently these are used only for data analysis)
  (defclass HumanAndCauseTextAFFNEG
    :super Text
    :equivalent (dl/and Text
                        (dl/some dul/hasComponent (dl/some indicatesRule HUMAN))
                        (dl/some dul/hasComponent (dl/some indicatesRule CAUSE))))

  (defclass AffirmedHumanCauseTextAFFNEG
    :super Text
    :equivalent (dl/and Text
                        (dl/some dul/hasComponent AffirmedHumanCauseTokenAFFNEG)))

  (defclass NegatedHumanCauseTextAFFNEG
    :super Text
    :equivalent (dl/and Text
                        (dl/some dul/hasComponent NegatedHumanCauseTokenAFFNEG)))


  (defclass NatureAndCauseTextAFFNEG
    :super Text
    :equivalent (dl/and Text
                        (dl/some dul/hasComponent (dl/some indicatesRule NATURE))
                        (dl/some dul/hasComponent (dl/some indicatesRule CAUSE))))

  (defclass AffirmedNaturalCauseTextAFFNEG
    :super Text
    :equivalent (dl/and Text
                        (dl/some dul/hasComponent AffirmedNaturalCauseTokenAFFNEG)))

  (defclass NegatedNaturalCauseTextAFFNEG
    :super Text
    :equivalent (dl/and Text
                        (dl/some dul/hasComponent NegatedNaturalCauseTokenAFFNEG)))


  ;; Account-level causality
  (defclass HumanCauseAccountAFFNEG
    :super OnlineAccount
    :equivalent (dl/and OnlineAccount
                        (dl/or
                          (dl/some publishes (dl/some dul/hasComponent AffirmedHumanCauseTokenAFFNEG))
                          (dl/some publishes (dl/some dul/hasComponent NegatedNaturalCauseTokenAFFNEG)))))

  (defclass NaturalCauseAccountAFFNEG
    :super OnlineAccount
    :equivalent (dl/and OnlineAccount
                        (dl/or
                          (dl/some publishes (dl/some dul/hasComponent AffirmedNaturalCauseTokenAFFNEG))
                          (dl/some publishes (dl/some dul/hasComponent NegatedHumanCauseTokenAFFNEG)))))

  (as-disjoint
    (defclass GreenHumanCauseAccountAFFNEG
      :super GreenAccount
      :equivalent (dl/and GreenAccount HumanCauseAccountAFFNEG))

    (defclass DenierHumanCauseAccountAFFNEG
      :super DenierAccount
      :equivalent (dl/and DenierAccount HumanCauseAccountAFFNEG)))


  (as-disjoint
    (defclass GreenNaturalCauseAccountAFFNEG
      :super GreenAccount
      :equivalent (dl/and GreenAccount NaturalCauseAccountAFFNEG))

    (defclass DenierNaturalCauseAccountAFFNEG
      :super DenierAccount
      :equivalent (dl/and DenierAccount NaturalCauseAccountAFFNEG))))


;;; --------------------------------------------------------------------------
;; FIXME: Identifying energy conservation accounts works differently via EnergyConservationText
;;
;;        Tawmy-OWL is coding (some oproperty (and obj1 obj2))
;;
;;                         as (and (some oproperty obj1)
;;                                 (some oproperty obj2))
;;
;; Discuss w/ RV and SR; then remove the old versions of theseclasses!
(defclass EnergyConservationAccountBROKEN1
  :super OnlineAccount
  :equivalent (dl/and OnlineAccount
                      (dl/some publishes (dl/and (dl/some dul/hasComponent EnergyToken))
                                                 (dl/some dul/hasComponent ConservationToken))))
(defclass EnergyConservationAccountBROKEN2
  :super OnlineAccount
  :equivalent (dl/and OnlineAccount
                      (dl/some publishes (dl/and Text
                                                 (dl/some dul/hasComponent EnergyToken))
                                                 (dl/some dul/hasComponent ConservationToken))))




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
(defn sum-analysis
  "Returns analysis element totals counts for all the tokens in a single text
  (or profile) or a sequence of them."
  [txt]
  (let [sum (fn [t]
              (reduce #(update-values %1 %2 (fnil inc 0))
                      {}
                      (remove empty? (:analysis t))))]
    ;; Have we one text or many?
    (cond
     (map? txt) (sum txt)
     (sequential? txt) (apply merge-with + (map sum txt)))))




;;; --------------------------------------------------------------------------
(defn sum-affect
  "Returns affect total counts for all the tokens in a text/profile."
  [txt]
  (select-keys (sum-analysis txt) Affect-Names))



;;; --------------------------------------------------------------------------
(defn echart-affect
  "Produces an affect stacked bar chart for all given text/profiles."
  [texts & opts]
  ;; Match affect colours to Incanter/jFree charting
  (let [affect [["Anger"        Color/red]
                ["Fear"         (new Color 000 153 000)]
                ["Sadness"      (new Color 148 000 211)]
                ["Joy"          (new Color 255 215 000)]
                ["Surprise"     (new Color 051 255 255)]
                ["Anticipation" (new Color 255 140 000)]
                ["Disgust"      (new Color 204 051 204)]
                ["Trust"        (new Color 051 255 102)]
                ["Positive"     Color/yellow]
                ["Negative"     (new Color 051 051 204)]]

        emote   (fn [[sname txts]]
                  ;; Affect levels across all texts for one user
                  (map (fn [[emo cnt]] [sname emo cnt])
                    (sum-affect txts)))

        ;; Group tweets by user, ordered by Z..A text count
        tcount  #(- (count (second %)))
        utexts  (sort-by tcount (group-by :screen_name texts))]

  (with-data (dataset [:user :emotion :level]                   ; dataset columns
                      (concat (map (fn [[emo _]] ["" emo 0])    ; Set colour order
                                   affect)
                       (mapcat emote utexts)))                  ; User affect levels

    (let [^JFreeChart chart (stacked-bar-chart
                             :user :level :group-by :emotion :legend true
                             :x-label "Users by decreasing activity"
                             :y-label "Emotion level")

          ^StackedBarRenderer rndr (-> chart
                                       .getCategoryPlot
                                       .getRenderer)]
      ;(view $data)

      ;; Set the colours for the order we made with the :sync rows
      (run! #(set-stroke-color chart (second (affect %)) :series %)
            (range (count affect)))

      ;; Render and go!
      (when (some #{:p100 :%} opts)
        (.setRenderAsPercentages rndr true))

      (view chart)))))




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
  or SASSY if none is configured."
  ([xmp]
  ;;; TODO: Handle multiple surveys if we're going to support more than one
  (etweet xmp (six/which-survey)))


  ([{:keys [tid
            polarity
            screen_name
            stance
            content
            analysis]}
    survey
    & opts]
  ;; The analysis includes sentiment, emotion, and SCRs. The latter are ignored.
  (let [snowball  (when survey
                    (tw/make-stemmer))
        colourize (fn [[word affect]]
                    (eword word affect survey snowball))
        etokens   (map colourize (zip content analysis))        ; Mark affect
        etext     (apply str (interpose \space
                                        (conj etokens           ; Tag tweet
                                              "\n"
                                              (log/<> tid (str screen_name stance)))))]
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
      :prefix  (prefix "ss")
      :import  say-sila
      :comment (str "Ontology for modelling '" oname "' climate change communications on Twitter."))))



;;; --------------------------------------------------------------------------
(defprotocol OntologyFactory
  "Functionality to create individual ontologies."
  (make-ontology-maker [o]
    "Returns a factory function which, depending on the :sila :community
    configuration setting, either returns a single ontology or a new one
    for each user."))

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
      (let [comm  (comm/new)
            onter (fn [sname]
                    (make-ontology (hyphenize otag sname)))]

        ;; Function to retrieve/create user ontologies in the community
        (fn [& [sname]]
          (case sname
           :community comm
           :size      (comm/size comm)
           :zap!      (comm/zap! comm)
           :fetch     (comm/fetch comm)
                      (comm/fetch comm sname onter))))

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
    {:keys [affect content tid pos-tags rules screen_name surveys]      ; Text breakdown
     :as   xmp}
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
    (when screen_name                                               ; Test data may not have screen names
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

            ;; Express survey concept rules
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
(defun finalize-activity
  "Populates an ontology community with user profile data from the 'examples'
  intermediate format. The function returns the ontology maker for the community."
  ([o activity texts]
  ;; Wait until we've finished processing ARFF data to create ontologies
  (log/debug "Finalizing activity" o)
  (await activity)

  (let [onter (make-ontology-maker o)                   ; Tag|ontology to factory function
        accts (into #{} (map #(vector (% :screen_name)  ; Find unique [userid stance] pairs
                                      (% :stance))
                        texts))]
    (run! (fn [[sname stance]]
            (let [ont (onter sname)]
              ;; Declare the Twitter user account (if we know it)
              (individual ont sname :type (case stance
                                            :green  GreenAccount
                                            :denier DenierAccount
                                                    OnlineAccount))))
          accts)
    ;; Return the ontology maker
    onter)))


;;; --------------------------------------------------------------------------
(defun populate-profiles
  "Populates an ontology community with user profile data from the 'examples'
  intermediate format. The function returns the ontology maker for the community."
  ([dtag users]
  (populate-profiles dtag users (cfg/? :sila)))


  ([o users sconf]
  (let [onter (make-ontology-maker o)]          ; Tag|ontology to factory function
    (when-not (:skip-profiles? sconf)
      (run! (fn [{:as xmp
                  sname :screen_name
                  descr :description              ; User profile text
                  tid   :tid}]                    ; Text ID is the profile ID
              (let [ont (onter sname)]
                ;; Add PoS/senti for the user's profile content
                  (add-text onter
                            (individual ont tid :type PersonalProfile)
                            xmp
                            sconf)))
            users))
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
  ;(log/debug "Adding dependencies:" tid)
  (let [ont     (onter screen_name)
        include #(refine ont %1 :fact (is dul/hasComponent %2))
        equiv?  #(or (= %1 %2)
                     (every? #{"\"" "QUOTE" "\"'" "'\"" "QUøTE"} [%1 %2]))
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
        (let [obj-num (try (Long/parseLong obj)
                        (catch Exception _
                               (log/error "Dependency parse @" tid ":" tok2 "(" sub "<-" obj ")")
                               -1))
              dep?    (pos? obj-num)
              build   #(conj % (if dep? obj-num nil))]
          ;; Complain if the POS analysis doesn't match up (uncommon)
          ;; TODO: This actually is more common than we'd like for large batches of tweets.
          ;;       We're not currently doing anything about it, and multiple warnings can hide
          ;;       the more serious tokenization error (below).  Therefore, it's currently OFF.
          (comment when (not= pos1 pos2 pos3)
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
  (let [onter (make-ontology-maker o)]
    ;; For testing, we may skip tweets (using only profiles)
    (when-not (:skip-statuses? sconf)
      ;; Populate combined|community ontology with the tweet statuses
      (run! #(add-text onter % sconf) xmps)

      ;; Add Tweebo dependencies
      (when (get sconf :use-tweebo?)
        ;; add-dependencies will update the ontology, but we need to do a bit more processing
        (twbo/wait)
        (let [xdeps (map #(add-dependencies onter %) xmps)]

          ;; Indicate which tokens were NOT negated
          (run! #(add-affirmations onter %) xdeps))))

    ;; Return the ontology look-er-up-er function
    onter)))


;;; --------------------------------------------------------------------------
(defn which-data
  "Returns the filepath for a user/text examples file."
  ([]
  (which-data @World))


  ([world]
  (:dtag world)))



;;; --------------------------------------------------------------------------
(defn which-edn
  "Returns the filepath for a user/text examples file."
  [dtag rsrc]
  (strfmt "~a/~a-~a.edn" World-FStub (name rsrc) (name dtag)))



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
  This function bundle is tuned by parameters in the :sila section of the
  configuration."
  []
  ;; Create a closure for a configuration-based analysis
  (let [all-pn? (cfg/?? :sila :skip-neutrals?)
        lex     (tw/make-lexicon (cfg/?? :sila :lexicon :liu))  ; TODO: Capture lex change on config update
        sball   (tw/make-stemmer)]                              ; Weka Affective Tweets plus Snowball stemmer

    ;; Bundle everything up
    (map->Toolbox
     {:all-pn?  (fn [] all-pn?)

      :stoic?   (fn [{:keys [analysis]}]                        ; Check that we're not including neutral Texts
                  (and all-pn? (every? empty? analysis)))       ; ..and that no sentiment (rule) is expressed

      :stem     (fn [w]
                  (.stem sball w))

      :sense    #(tw/analyze-token+- lex % Affect-Fragments)    ; Lexicon lookup for P/N rules

      :scr      #(reduce (fn [acc [scr terms]]                  ; Match terms for Sentiment Composite Rules
                             (if (six/in-stems? terms % sball some)
                                 (conj acc scr)
                                 acc))
                           #{}
                           Rule-Stems)

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


  ([tools tid sname elements stance]
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
     :stance      (keyword stance)
     :tid         tid
     :content     terms
     :affect      affect
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
  (let [target  (dset/col-target :u)
        insts   (weka/load-dataset data target)
        dtag    (dset/Datasets :u)                              ; Structure for user (U99) data
        tools   (toolbox)                                       ; Sentiment/emotion analysis
        attrs   (select-keys (dset/Columns dtag) [:screen_name  ; 0-based attribute indices
                                                  :name
                                                  :description
                                                  target])]
    ;; Create a sequence of maps, each representing a Weka instance
    (map #(update % target keyword)                             ; Lazily convert "?" to :?
         (reduce
           (fn [acc ^Instance inst]
             (let [avals (update-values attrs #(.stringValue inst (int %)))     ; Pull attr-vals
                   sname (:screen_name avals)
                   tid   (str lbl/Profile-Tag sname)                            ; TextID is profile name
                   xmp   (make-example tools tid sname                          ; Check emotion
                                       (avals :description)
                                       (avals target))]
             ;; Add on hashmap with attribute data plus emotion analysis
             (conj acc (merge avals xmp))))
         '()
         (weka/instance-seq insts)))))


  ([dtag data]
  (hash-map :dtag  dtag
            :users (profiles->examples data))))



;;; --------------------------------------------------------------------------
(defn statuses->examples
  "Converts Weka status (S99) instances (tweets) into a sequence of examples
  in the intermediate format."
  [dtag data]
  ;; Keep track of how many examples to create, as per the configured 'balance' setting
  (let [insts        (weka/load-dataset data (dset/col-target :s))
        columns      (dset/columns :s)
        [col-id
         col-sname
         col-text
         col-target] (map columns [:id :screen_name :text (dset/col-target :s)])
        tools        (toolbox)
        activity     (agent {})]                            ; Track user text counts

    ;; The number of examples we're creating depends on how things were configured
    (log/info "Converting" (.numInstances insts) "instances")

    ;; Shall we (pseudo)randomize the instances?
    (when-let [seed (cfg/?? :sila :rand-seed)]
      (log/fmt-info "Shuffling ~a input instances: seed[~a]" (.numInstances insts) seed)
      (.randomize insts (Random. seed)))

    ;; Throw away the counter & return the folded example sequence
    (hash-map
     :dtag dtag
     :activity activity
     :texts (domap (fn [^Instance inst]
                     (let [tid    (lbl/label-text (.stringValue inst (int col-id)))
                           sname  (.stringValue inst (int col-sname))
                           stance (.stringValue inst (int col-target))  ; ARFF target is "stance"
                           elms   (.stringValue inst (int col-text))]   ; Text elements are "PoS_term"

                       (send activity #(update % sname (fnil inc 0)))   ; Prepare for filtering
                       (make-example tools tid sname elms stance)))     ; Example as a hashmap

                   (weka/instance-seq insts)))))                        ; SEQ: Weka instances



;;; --------------------------------------------------------------------------
(defn filter-by-activity
  "Takes a stream of statuses or user profiles and returns a lazy sequence
  containing only those who have published at least as many tweets as the
  :min-statuses parameter in the :sila configuration."
  ([world]
  (filter-by-activity world (cfg/? :sila)))


  ([{:keys [activity users texts]
     :as world}
    sconf]
  ;; Keep profiles/statuses with at least the minimum number of tweets
  (let [counts @activity
        thresh (sconf :min-statuses 1)
        select (fn [texts]
                 (filter #(when-let [n (counts (:screen_name %) 0)]
                            (>= n thresh))
                         texts))]
    (merge world {:users (select users)
                  :texts (select texts)}))))



;;; --------------------------------------------------------------------------
(defn zap-activity
  "Reinitializes the status activity agent."
  ([]
  (zap-activity @World))

  ([{:keys [activity]}]
  (await activity)
  (send activity (fn [_] {}))))



;;; --------------------------------------------------------------------------
(defn create-world
  "Returns a world structure containing the official say-sila examples,
  ontologies and associated data."
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
    (create-world dtag ausers atexts)))


  ([dtag]
  ;; Reuse a saved set of examples (rather than constructing news ones per the configuration)
  (let [fpath (which-edn dtag :examples)]
    (if (.exists (io/file fpath))
        (do (log/fmt-info "User/text examples~a: ~a" dtag fpath)
            (create-world (edn/read-string (slurp fpath))
                          (cfg/? :sila {})))
        (log/warn "No saved world:" (name dtag)))))


  ([{:keys [dtag activity]
     :as   preworld}
    sconf]
  ;; This middle arity clause is where we wrap everything up!
  ;; The (pre)world already has users and their texts; create the ontology.
  (let [{:as   world
         :keys [users texts]} (filter-by-activity preworld sconf)

        onter (-> (finalize-activity dtag activity texts)
                  (populate-profiles users sconf)
                  (populate-statuses texts sconf))]

    ;; Put everything together
    (assoc world :ontology onter)))


  ([dtag ausers atexts]
  (create-world dtag ausers atexts (cfg/? :sila {})))


  ([dtag ausers atexts sconf]
  ;; Create e[x]amples from the source [a]arff files
  (log/fmt-info "Dataset user~a: ~a" dtag ausers)
  (log/fmt-info "Dataset text~a: ~a" dtag atexts)

  (create-world (merge (profiles->examples dtag ausers)     ; U-dataset user profiles
                       (statuses->examples dtag atexts))    ; S-dataset tweet status texts
                  sconf)))



;;; --------------------------------------------------------------------------
(defun create-world!
  "Loads the official say-sila examples and ontologies into the official World
  for this namespace.  Contrary to the generalized create-world, this function
  simply returns the data tag for the new official World.  This behaviour is
  due to the complexity of worlds and the function's intended use in the REPL."
  [& args]
  (let [w (apply create-world args)]
    (reset! World w)
    (send Memory conj [:world (jvm/memory-used :MB)])
    (:dtag w)))


;;; --------------------------------------------------------------------------
(defn get-accounts
  "Returns a set of the user accounts in a world.  The users may be pulled
  with respect to a text type of :texts for tweets (default) or :users for
  user profiles."
  ([]
  (get-accounts :texts))


  ([ttype]
  (get-accounts ttype @World))


  ([ttype world]
  (into #{} (map :screen_name (world ttype)))))



;;; --------------------------------------------------------------------------
(defn get-user-texts
  "Returns a sequence of text examples from the user of the specified world.
  These texts can be tweets (ttype is :texts) or profiles (ttype is :users)."
  ([user]
  (get-user-texts user :texts))


  ([user ttype]
  (get-user-texts user ttype @World))


  ([user ttype world]
  (filter #(= user (:screen_name %)) (world ttype))))



;;; --------------------------------------------------------------------------
(defun count-affect
  "Returns a map containing the counts of affective words in tweets for the
   following user categories: all, green, denier."
  ([]
  (count-affect @World))


  ([world :guard map?]
  (count-affect (:texts world)))


  ([texts]
  (let [;; Initialize map, keyed by affect with zero counts
        zeros (apply zero-hashmap Affect-Names)

        ;; Functions to count affect at each level: word -> tweet -> series
        emote-token (fn [cnts emos]
                      (update-values cnts (map str emos) inc))

        emote-text  (fn [cnts txt]
                      (reduce emote-token cnts (:affect txt)))

        emote       (fn [txts]
                      (reduce emote-text zeros txts))

        ;; Filter tweet sequences by user type
        flt-stance  (fn [s]
                      (filter #(= s (:stance %)) texts))

        texts-map   (hash-map :all    texts
                              :green  (flt-stance :green)
                              :denier (flt-stance :denier))]

    ;; Now, count all that up!
    (update-values texts-map emote))))



;;; --------------------------------------------------------------------------
(defn count-user-affect
  "Returns a map containing the counts of affective words in texts for a single
  user in the world."
  ([user]
  (count-user-affect user :texts))


  ([user ttype]
  (count-user-affect user ttype @World))


  ([user ttype world]
  (:all (count-affect (get-user-texts user ttype world)))))



;;; --------------------------------------------------------------------------
(defn count-world-affect
  "Returns a map containing the counts of affective words in texts, keyed by
  the users publishing the texts."
  ([]
  (count-world-affect :texts))


  ([ttype]
  (count-world-affect ttype @World))


  ([ttype world]
  ;; We get a 3X speedup using pmap over map
  (into {} (pmap (fn [acct]
                   [acct (count-user-affect acct ttype world)])
                 (get-accounts ttype world)))))



;;; --------------------------------------------------------------------------
(defn find-user-indicators
  "Returns a set containing the ontological symbols representing the say.sila
  indicator accounts which apply to the specified user."
  ([user]
  (find-user-indicators user @World))


  ([user {onter :ontology}]
  (when onter
    ;; Use local symbols when called from another namespace
    (inf/with-ns-silence 'say.sila
      (let [inds '[WeakHumanCauseAccount            StrongHumanCauseAccount
                   WeakNatureCauseAccount           StrongNatureCauseAccount
                   WeakEnergyConservationAccount    StrongEnergyConservationAccount
                   WeakCO2CutAccount                StrongCO2CutAccount
                   WeakEnvironmentProtectAccount    StrongEnvironmentProtectAccount
                   WeakEconomicGrowthAccount        StrongEconomicGrowthAccount]
            ont  (onter user)
            hits (reduce #(if (empty? (rsn/instances ont (eval %2)))
                              %1
                              (conj %1 %2))
                         #{}
                         inds)]
        (when ont
          (inf/unreason ont)                        ; Reclaim memory from reasoner
          (into #{} (filter hits inds))))))))



;;; --------------------------------------------------------------------------
(defn find-indicators
  ""
  ([]
  (find-indicators :texts))


  ([ttype]
  (find-indicators ttype @World))


  ([ttype world]
  (into {} (pmap #(vector %
                          (find-user-indicators % world))
                 (get-accounts ttype world)))))



;;; --------------------------------------------------------------------------
(defn get-user-stance
  "Returns the user's stance on climate change per the recorded Weka analysis
  of his/her tweets."
  ([user]
  (get-user-stance user @World))


  ([user world]
  ;; As a sanity check, make sure the user has only one stance
  (let [stances (into #{} (map :stance (get-user-texts user :texts world)))]
    (if (at-most? 1 stances)
        (first stances)
        (log/error "User" user "has multiple stances:" stances)))))



;;; --------------------------------------------------------------------------
(defn world->arff
  "Creates an ARFF representing the specified world.  The caller may indicate
  a text type of :text (tweets) :users (profiles) or :all (TODO)."
  ([]
  (world->arff :texts))


  ([ttype]
  (world->arff ttype @World))


  ([ttype world]
  ;; TODO: Adapt and move this function to weka.dataset as Y00
  (let [insts   (weka/load-dataset (str Data-Plan-Dir "/G01.arff") "stance")
        [uid &                                      ; screen name
         attrs] (weka/attribute-seq insts)          ; Everything else (skips target)

        attrcnt (.numAttributes insts)
        target  (dset/col-target :g)
        relname (str (.relationName insts) "-"
                     (name (:dtag world))  "-"
                     (cfg/?? :sila :min-statuses))

        ;; Combine affect & indicators into a account-keyed map
        _       (log/debug "Counting affect")
        affects (count-world-affect ttype world)

        _       (log/debug "Finding indicators")
        indics  (find-indicators ttype world)

        _       (log/debug "Getting user stances")
        stances (into {} (pmap (fn [acct]
                                 [acct (get-user-stance acct world)])
                               (keys affects)))

        _       (log/debug "Merging affect and indicators")
        users   (merge-with merge affects
                                  (p-update-values indics
                                                   #(into {} (map (fn [acct]
                                                                    ;; Convert set entries to {key=>1}
                                                                    [(name acct) 1])
                                                                  %))))]
    ;; Create the new dataset
    (log/notice "Creating ARFF:" relname)
    (.setRelationName insts relname)
    (doseq [[usr data] users]
      (let [inst    (DenseInstance. attrcnt)
            setdata (fn [^Attribute attr]
                      (.setValue inst attr
                                 (double (get data (.name attr) 0.0))))]
        (.setDataset inst insts)
        (.setClassValue inst (name (stances usr)))
        (.setValue inst ^Attribute uid
                        ^String usr)
        (run! setdata attrs)
        (.add insts inst)))

    ;; TODO: Determine where the ARFF should go
    (weka/save-file (str Tmp-Dir "/" relname ".arff") insts))))



;;; --------------------------------------------------------------------------
(defn- report-to-csv
  "Saves the specified information to a CSV file."
  [ctype concept syms fullcnt percents]
    ;; Determine to the the appropriate CSV for this concept
    (let [ctype   (str/capitalize (name ctype))
          ->title #(if (string? %)
                       (str/capitalize %)
                       (soc/acronymize %))
          minimum (cfg/?? :sila :min-statuses)
          csv     (str Tmp-Dir "/" concept ctype ".csv")
          exists? (fs/exists? csv)]                       ; Know existance BEFORE opening
      (with-open [wtr (io/writer csv :append true)]
        ;; Handle header for a new CSV
        (when-not exists?
          (log/notice "Creating report:" csv)
          (.write wtr (strfmt "Min Tweets,~a~{,~a~}~%" ctype (map ->title syms))))
        ;; Report one line of CSV data
        (.write wtr (strfmt "~a,~a~{,~a~}~%" minimum fullcnt percents)))))



;;; --------------------------------------------------------------------------
(defn report-examples
  "Give positive/negative coverage and sentiment statistics for sets of
  intermediate-format examples or a hashmap with multiple sets of examples
  as values.  See report-world for a list of reporting options accepted by
  the final clause."
  ([]
  (report-examples @World))


  ([{:keys [dtag
            texts]}]
  (report-examples dtag texts :texts))


  ([dtag texts text-type & opts]
  (let [;; Statistics on user stance and presence of affect
        ttype   (name text-type)
        stats   (reduce #(let [ss (if (every? empty? (:affect %2))
                                      :stoic
                                      :senti)]
                           (update-values %1 [:count (:stance %2) ss] inc))
                        (zero-hashmap :count :green :denier :? :senti :stoic)
                        texts)
        fullcnt (stats :count)

        ;; Generalized roll-up functionality
        stat100 #(p100z (stats %) fullcnt)

        zero    #(apply zero-hashmap %)
        init    #(vector (map %1 texts)                         ; [elements, initial count-map]
                         (zero %2))
        kount   #(update-values %1 %2 inc)                      ; Accumulate hits from seq %2

        count-tokens    (fn [zeros elements]                    ; TODO: use sum-analysis
                          (reduce kount
                                  zeros
                                  (flatten (map #(remove empty? %) elements))))

        count-texts     (fn [zeros elements]
                          (reduce #(update-values %1 (apply set/union %2) inc)
                                  zeros
                                  elements))

        count-all       (fn [elm items]
                          (let [[rules
                                 zeros] (init elm items)]
                            [(count-tokens zeros rules)
                             (count-texts  zeros rules)]))

        report          (fn [keyz toks txts what show width]
                          (log/debug)
                          (domap
                            #(let [tokcnt (get toks % 0)
                                   txtcnt (get txts % 0)
                                   pct    (pctz txtcnt fullcnt)]
                               (log/fmt-debug "~a~a ~va [~4d tokens in ~4d ~a (~5,2F%)]"
                                              what dtag width (show %)
                                              tokcnt txtcnt ttype (* 100 pct))
                               pct)
                            keyz))

        ;; Calculate token & text counts for key elements
        [aff-toks aff-txts] (count-all :affect  Affect-Names)           ; Pos/neg & emotions
        [svy-toks svy-txts] (count-all :surveys (keys Surveys))         ; Six Americas surveys
        [scr-toks scr-txts] (count-all :rules   (keys Rule-Triggers))   ; Survey Concept Rules

        ;; Now get a sequence of part-of-speech tags for the Texts.
        [pos-tags                                               ; POS tags for all Texts
         pos-zeros] (init :pos-tags pos/POS-Codes)              ; Acc init: POS tag counts

        pos-toks (reduce kount pos-zeros pos-tags)
        pos-txts (reduce (fn [cnts pos]
                           (update-values cnts (into #{} pos) inc))
                         pos-zeros
                         pos-tags)]

  ;; Report the basic statistics
  (log/fmt-info "Dset:~a: grn[~1$%] dnr[~1$%] s[~1$%] txts~a"
                dtag (stat100 :green) (stat100 :denier) (stat100 :senti) stats)

  ;; Report pos/neg first, then the emotions
  (report (conj (sort (keys (dissoc aff-txts "Positive" "Negative")))   ; ABCize emotions
                "Negative"                                              ; Add onto head
                "Positive")                                             ; ..of the list
          aff-toks aff-txts "Affect" identity 12)

  ;; Six Americas surveys
  (report (sort (keys svy-txts))
          svy-toks svy-txts "Survey" name 12)

  ;; Survey Concept Rules (show in REPL and add to running CSV files)
  (doseq [[concept symbols] [["CauseBeliever"   ["CAUSE" "HUMAN" "NATURE" "NEGATION"]]
                             ["Conservation"    ["ENERGY" "CONSERVATION"]]
                             ["CO2Cut"          ["CO2" "CUT"]]
                             ["EnvProtect"      ["ENVIRONMENT" "PROTECT"]]
                             ["EconGrowth"      ["ECONOMIC" "GROWTH"]]
                             ["PeopleHarm"      ["PEOPLE" "HARM"]]
                             ["CompanyReward"   ["COMPANY" "REWARD"]]   ; TODO: remove (low coverage)
                             ["CompanyPunish"   ["COMPANY" "PUNISH"]]   ; TODO: remove (low coverage)
                            ]]
    ;; Always report to REPL
    (let [pcts (report symbols scr-toks scr-txts "Concept" identity 12)]

      ;; And report tweets to the CSV
      (when (= text-type :statuses)
        (report-to-csv "Tokens" concept symbols fullcnt pcts))))

  ;; Report part-of-speech tags
  (when-not (some #{:no-pos} opts)
    (report (sort-by pos/POS-Fragments
                     (keys pos-txts))
            pos-toks pos-txts "Speech" pos/POS-Fragments 24)))))



;;; --------------------------------------------------------------------------
(defn report-concepts
  "Gives instance coverage of concepts from say-sila community ontologies."
  ([]
  (report-concepts @World))


  ([{:keys [dtag ontology texts]
     :as   world}]
  ;; TODO: The newer community way is subtly different from the original say-sila world.
  ;;       Handle non-community mode as a signle-ontology community.
  (let [txtcnt   (count texts)
        [onts
         usrcnt] (if (cfg/?? :sila :community?)
                     [(ontology :fetch)     , (ontology :size)]         ; A set of user ontologies
                     [[((:ontology world))] , (count (:users world))])] ; [big ontology with all users]

    (report-concepts dtag onts {:texts txtcnt
                                :users usrcnt})))


  ([dtag onts fullcnts]
  ;; Use local symbols when called from another namespace
  (binding [*ns* (find-ns 'say.sila)]
    (let [;; The concept map is organized according to the report setup:
          ;;         LEVEL  CONCEPT-TAG         ONTOLOGY SYMBOLS
          concepts {[:users "LeaderFollowers"]  '[GreenAccount
                                                  DenierAccount]

                    [:texts "Conservation"]     '[WeakEnergyConservationText
                                                  StrongEnergyConservationTextAB]

                   ; FIXME: Make a decision about the AFFIRM-NEGATE strategy
                   ;        so that we may remove these stragglers!
                   ;[:texts "CauseAFFNEG"]      '[HumanAndCauseTextAFFNEG
                   ;                              AffirmedHumanCauseTextAFFNEG
                   ;                              NegatedHumanCauseTextAFFNEG
                                                  ;---------------------------------
                   ;                              NatureAndCauseTextAFFNEG
                   ;                              AffirmedNaturalCauseTextAFFNEG
                   ;                              NegatedNaturalCauseTextAFFNEG]

                   ;[:users "CauseAFFNET"]      '[HumanCauseAccountAFFNEG
                   ;                              GreenHumanCauseAccountAFFNEG
                   ;                              DenierHumanCauseAccountAFFNEG
                                                  ;-----------------------------------------
                   ;                              NaturalCauseAccountAFFNEG
                   ;                              GreenNaturalCauseAccountAFFNEG
                   ;                              DenierNaturalCauseAccountAFFNEG]

                    [:users "HumanCause"]       '[WeakHumanCauseAccount
                                                  GreenWeakHumanCauseAccount
                                                  DenierWeakHumanCauseAccount
                                                  ;-----------------------------------------
                                                  StrongHumanCauseAccount
                                                  StrongHumanCauseAccountAB
                                                 ;StrongHumanCauseAccountBA   ; Small percentage
                                                  GreenStrongHumanCauseAccount
                                                  DenierStrongHumanCauseAccount]

                    [:users "NatureCause"]      '[WeakNatureCauseAccount
                                                  GreenWeakNatureCauseAccount
                                                  DenierWeakNatureCauseAccount
                                                  ;-----------------------------------------
                                                  StrongNatureCauseAccount
                                                  StrongNatureCauseAccountAB
                                                  GreenStrongNatureCauseAccount
                                                  DenierStrongNatureCauseAccount]

                    [:users "Conserv-OLD"]      '[EnergyConservationAccountBROKEN1
                                                  EnergyConservationAccountBROKEN2]

                    [:users "Conservation"]     '[WeakEnergyConservationAccount
                                                  GreenWeakEnergyConservationAccount
                                                  DenierWeakEnergyConservationAccount
                                                  ;-----------------------------------------
                                                  StrongEnergyConservationAccount
                                                  StrongEnergyConservationAccountAB
                                                  GreenStrongEnergyConservationAccount
                                                  DenierStrongEnergyConservationAccount]

                    [:users "CO2Cut"]           '[WeakCO2CutAccount
                                                  GreenWeakCO2CutAccount
                                                  DenierWeakCO2CutAccount
                                                  ;-----------------------------------------
                                                  StrongCO2CutAccount
                                                  StrongCO2CutAccountAB
                                                  GreenStrongCO2CutAccount
                                                  DenierStrongCO2CutAccount]
                                                  ;-----------------------------------------

                    [:users "EnvProtect"]       '[WeakEnvironmentProtectAccount
                                                  GreenWeakEnvironmentProtectAccount
                                                  DenierWeakEnvironmentProtectAccount
                                                  ;-----------------------------------------
                                                  StrongEnvironmentProtectAccount
                                                  StrongEnvironmentProtectAccountAB
                                                  GreenStrongEnvironmentProtectAccount
                                                  DenierStrongEnvironmentProtectAccount]

                    [:users "EconGrowth"]       '[WeakEconomicGrowthAccount
                                                  GreenWeakEconomicGrowthAccount
                                                  DenierWeakEconomicGrowthAccount
                                                  ;-----------------------------------------
                                                  StrongEconomicGrowthAccount
                                                  StrongEconomicGrowthAccountAB
                                                  GreenStrongEconomicGrowthAccount
                                                  DenierStrongEconomicGrowthAccount]
                                                  ;-----------------------------------------

                    [:users "PeopleHarm"]       '[WeakPeopleHarmAccount
                                                  GreenWeakPeopleHarmAccount
                                                  DenierWeakPeopleHarmAccount
                                                  ;-----------------------------------------
                                                  StrongPeopleHarmAccount
                                                  StrongPeopleHarmAccountAB
                                                  GreenStrongPeopleHarmAccount
                                                  DenierStrongPeopleHarmAccount]
                                                  ;-----------------------------------------
                    ;; TODO: Remove trial with low coverage
                    [:users "CompanyReward"]    '[WeakCompanyRewardAccount
                                                  GreenWeakCompanyRewardAccount
                                                  DenierWeakCompanyRewardAccount
                                                  ;-----------------------------------------
                                                  StrongCompanyRewardAccount
                                                  StrongCompanyRewardAccountAB
                                                  GreenStrongCompanyRewardAccount
                                                  DenierStrongCompanyRewardAccount]
                                                  ;-----------------------------------------

                    ;; TODO: Remove trial with low coverage
                    [:users "CompanyPunish"]    '[WeakCompanyPunishAccount
                                                  GreenWeakCompanyPunishAccount
                                                  DenierWeakCompanyPunishAccount
                                                  ;-----------------------------------------
                                                  StrongCompanyPunishAccount
                                                  StrongCompanyPunishAccountAB
                                                  GreenStrongCompanyPunishAccount
                                                  DenierStrongCompanyPunishAccount]
                                                  ;-----------------------------------------

                    [:users "Inferred1"]        '[WeakInferredGreenAccount1
                                                  GreenWeakInferredGreenAccount1
                                                  DenierWeakInferredGreenAccount1
                                                  StrongInferredGreenAccount1
                                                  GreenStrongInferredGreenAccount1
                                                  DenierStrongInferredGreenAccount1]

                    [:users "Inferred2"]        '[WeakInferredGreenAccount2
                                                  GreenWeakInferredGreenAccount2
                                                  DenierWeakInferredGreenAccount2
                                                  StrongInferredGreenAccount2
                                                  GreenStrongInferredGreenAccount2
                                                  DenierStrongInferredGreenAccount2]}

          needles (comm/instances onts (mapcat val concepts))

          report  (fn [sym what fullcnt]
                    ;; Report to the REPL console
                    (let [elms (get needles sym)
                          cnt  (count elms)
                          pct  (pctz cnt fullcnt)]
                      (log/fmt-info "~a~a: ~a of ~a ~a (~,2F%)"
                                    sym dtag cnt fullcnt what (* 100 pct))
                      (comment run! #(log/debug "  -" (iri-fragment %)) elms)
                      pct))

          rpt-csv (fn [[[level concept] syms]]
                    (log/debug)
                    ;; Report to the the appropriate CSV for this concept
                    (let [lhead   (str/capitalize (name level))       ; Title case for header/filename
                          fullcnt (fullcnts level)                    ; Pull text|user count
                          pcts    (domap #(report % lhead fullcnt) syms)]
                      (report-to-csv level concept syms fullcnt pcts)))]

      ;; Log report to the console & file for all targets
      (run! rpt-csv concepts)))))



;;; --------------------------------------------------------------------------
(defn report-world
  "Give positive/negative/emotion/survey/part-of-speech coverage for the
  Say-Sila World data. Supported options are:
    :users  - list the users for each data tag
    :no-pos - omit the part of speech report"
  [& opts]
  (let [{:keys [dtag
                texts
                users]} @World]

    ;; Report affect and PoS in profiles and tweets
    (run! (fn [[txts ttype title]]
            (log/info title)
            (apply report-examples dtag txts ttype opts)
            (log/debug))
          [[users :profiles "User Profiles:"]
           [texts :statuses "User Tweets:"]])

    (when (some #{:users} opts)
      (log/debug)
      (log/fmt-info "USERS~a: ~a" dtag (map :screen_name users)))))



;;; --------------------------------------------------------------------------
(defn eprint-texts
  "Prints a colourized representions of the specified class of texts with
  the associated dependency trees."
  ([txtsym]
  (eprint-texts txtsym nil nil))

  ([txtsym arg]
  (cond
   (string? arg) (eprint-texts txtsym arg nil)      ; (Sequence of) account(s)
   (number? arg) (eprint-texts txtsym nil arg)))    ; Max tweets to eprint

  ([txtsym accts n]
  (eprint-texts txtsym accts n @World))

  ([txtsym
    accts
    n
    {:as    world
     :keys  [dtag texts]}]
  (log/fmt-info "Searching for ~a across the community..." (if (symbol? txtsym)
                                                               txtsym
                                                               (iri-fragment txtsym)))
  (let [onter (:ontology world)
        onts  (if accts
                  (map #(onter %) (seqify accts))
                  (onter :fetch))
        tids (into #{} (map iri-fragment
                            (comm/instances onts txtsym)))
        txts (filter #(contains? tids (:tid %)) texts)]

    ;; Print the (subset of n) colour-coded tweets & the dependency hierarchy
    (run! eprint-tweet (sort-by :screen_name (shuffle (if n
                                                          (take n txts)
                                                          txts)))))))



;;; --------------------------------------------------------------------------
(defn eprint-user
  "Pretty-prints a user's profile and tweets, highlighting the affect and
  showing a token dependency tree if available."
  ([user]
  (eprint-user user @World))


  ([user
    {:as    world
     :keys  [dtag]}]
  ;; Run through the supported text-types
  (run! (fn [ttype]
          ;; Group profile/tweet reports by the dataset
          (run! (fn [txt]
                  (log/fmt! "~a'~a' ~a ~a~a\n" log/Bright
                                               (name dtag)
                                               (case ttype :users "Profile of"
                                                           :texts "Tweets for")
                                               user
                                               log/Text)
                  (eprint-tweet txt))
                (get-user-texts user ttype world)))
        [:users :texts])))



;;; --------------------------------------------------------------------------
(defn echart-user
  "Produces an affect stacked bar chart for all text published by the user."
  [user & args]
  (let [[world
         opts]  (optionize map? @World args)
        texts   (:texts world)]
    ;; Grab this user's texts and chart 'em!
    (apply echart-affect (filter #(= user (:screen_name %)) texts) opts)))



;;; --------------------------------------------------------------------------
(defn echart-text-instances
  "Produces an affect stacked bar chart for all texts associated with the
  specified Text class."
  [klass & args]
  (let [[{:keys  [texts ontology]
           :as    world}
         opts]  (optionize map? @World args)

        ;; FIXME: Handle non-community mode as a single-ontology community.
        onts (if (cfg/?? :sila :community?)
                 (ontology :fetch)
                 [((:ontology world))])

        ;; Find the text IDs for texts of the specified subclass
        tids (into #{}
                   (map iri-fragment (comm/instances onts klass)))]

    ;; Chart just those texts for the users
    (apply echart-affect (filter #(contains? tids (:tid %)) texts) opts)))



;;; --------------------------------------------------------------------------
(defn save-accounts
  "Saves a list of the accounts in the specified world."
  ([]
  (save-accounts (str Tmp-Dir "/accounts.lst")))


  ([fpath]
  (save-accounts @World fpath))


  ([{onter :ontology}
    fpath]
  ;; An ontology community is keyed by the user screen names
  (let [comm  (onter :community)
        accts (filter some? (keys @comm))]  ; FIXME: We have an extra nil key with an empty ontology
     (with-open [wtr (io/writer fpath)]
       (doseq [a accts]
         (.write wtr (str a "\n"))))

     ;; Just return some information on what we saved
     {fpath (count accts)})))



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


  ([{:keys  [dtag ontology]
     :as    world}]
  (save-ontology say-sila Ont-FPath :owl)
  (if (cfg/?? :sila :community?)
      (comm/save (ontology :community) dtag)
      (merge {:say-sila Ont-FPath}
             (save-ontology-map (:ontology world) Ont-FStub)))))



;;; --------------------------------------------------------------------------
(defn save-world
  "Saves the World example information.  Note that you must specify the :ont
  option to also save the associated ontologies."
  [& opts]
  (let [world @World
        dtag  (which-data)
        fpath (which-edn dtag :examples)]

    (spit fpath (pr-str (select-keys world [:users :texts])))

    ;; Return a map of the files saved
    (merge {:examples fpath}
           (when (some #{:ont} opts)
             (save-ontologies world dtag)))))



;;; --------------------------------------------------------------------------
(defn run
  "Performs an experiment."
  [dtag ausers atexts]
  (let [maxmin  4                                   ; Maximum 'minimum activity'
        sconf   (cfg/? :sila)
        yn??    [true false]

        ;; Set up to skip profiles (0), then statuses, then neither
        [_ skip1 & skips] (for [profs yn??
                                stats yn??]
                            {:skip-profiles? profs, :skip-statuses? stats})]

    (letfn [;; ---------------------------------------------------------------
            (reconf
              ([n]
                (reconf n skip1))

              ([n skips]
                (merge sconf skips {:min-statuses n})))

            ;; ---------------------------------------------------------------
            (zap! [w]
              ;; Release the community ontologies to free up memory
              ((w :ontology) :zap!))

            ;; ---------------------------------------------------------------
            (reworld [w & cfg]
              (create-world w (apply reconf cfg)))

            ;; ---------------------------------------------------------------
            (report
              ([w]
                (report-concepts w)
                (zap! w))

              ([w0 n]
                (report w0)
                (run! #(report (reworld w0 n %)) skips)
                "\n"))]

      ;; Check ontological coverage for a series of increasing minimum activity
      (loop [n 1
             w (create-world dtag ausers atexts (reconf 1))]

        (when (<= n maxmin)
          (log/info "Minimum status count:" n)
          (log/info "Community size:" ((w :ontology) :size))

          (let [rpt (report w n)
                n+1 (inc n)]
          (log/debug rpt)
          (recur n+1 (reworld w n+1))))))))



;;; --------------------------------------------------------------------------
(fs/mkdir Tmp-Dir)

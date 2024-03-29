;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Hierarchical reasoning architecture.
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.hierarchy
  (:require [say.genie  :refer :all]
            [say.config :as cfg]
            [say.log    :as log]
            [say.sila   :as sila]
            [weka.core  :as weka]
            [weka.tweet :as wtw]
            [clojure.core.async :as a :refer [>! <! >!! <!! chan go-loop]]
            [clojure.string     :as str])
  (:import  [weka.core Attribute
                       Instances]
            [weka.filters.unsupervised.attribute TweetToInputLexiconFeatureVector   ; emo/bws
                                                 TweetToGenderFeatures]))

(set! *warn-on-reflection* true)

(def ^:const BUFFER     16)
(def ^:const CONN-KEYS  [:ml-sig :ml-gnd-sig :ml-gnd-fbk :dl-gnd-sig])


(defonce Inua (atom nil))                   ; Possessor/master/spirit/soul [Inuit]
(defonce Tell (atom nil))


;;; --------------------------------------------------------------------------
(defn connect
  "Creates a channel for inter level/layer communications."
  ([]
  (chan BUFFER))

  ([chan-keys]
  (reduce #(assoc %1 %2 (connect)) {} chan-keys)))



;;; --------------------------------------------------------------------------
(defn- shutdown
  "Logs a shutdown notification.  If the caller specifies an channel, we
  forward the quit command."
  ([tag]
  (log/notice "Shutting down" (KEYSTR tag) "layer"))

  ([tag out]
  (shutdown tag)
  (>!! out :quit)))



;;; --------------------------------------------------------------------------
(defn shutdown!
  "Shuts down the running hierarchy."
  []
  (doseq [[_ c] @Inua]
    (>!! c :quit))

  (doseq [a [Inua Tell]]
    (reset! a nil)))



;;; --------------------------------------------------------------------------
(defn- go-DL
  "Starts up a description-logic layer.  This layer currently has no output.
  The application may see its work reflected in the say-sila ontology."
  [conns]
  ;; TODO: Break out various component processes
  (let [{gender  :dl-gnd-sig
         gnd-fbk :ml-gnd-fbk} conns]

    ;; Processing loop for the description logic layer.
    (go-loop [msg (<! gender)]
      (when-not (= :quit msg)
        (log/info "DL:" msg)
        (let [{[dom rng] :tweets} msg]
          ;; Make sure the user is in the ontology and feed back her F/M tweet counts
          (sila/alter-ontology :tweets dom rng)
          (>! gnd-fbk 0)                        ;TODO: (sila/get-gender-tweet-counts dom))
          ; TODO: (<! gender)
          )

        (recur (<! gender))))))



;;; --------------------------------------------------------------------------
(defn- go-ML-gender
  "Starts up the gender module in the machine-learning level."
  [conns ecfg]
  (let [{signal   :ml-gnd-sig                       ; << emotional instances
         feedback :ml-gnd-fbk
         output   :dl-gnd} conns
         id-ndx     (weka/index1->0 (:id-index ecfg))
         sname-ndx  (weka/index1->0 (:screen-name-index ecfg))
         genderer   (wtw/prep-emoter (TweetToGenderFeatures.) ecfg)]

    ;; We receive Weka instances on our signal channel
    (go-loop [einsts (<! signal)]
      (when-not (= :quit einsts)
        (let [ginsts  (weka/filter-instances einsts genderer)
              ginst   (.firstInstance ginsts)
              twid    (.stringValue ginst id-ndx)
              sname   (.stringValue ginst sname-ndx)
              ;FIXME: Insert pre-trained Weka model here!
              gender  (["FEMALE" "MALE"] (rand-int 2))
             ;fm-cnts (sila/get-gender-tweet-counts screen-name)
        ]

          ;; A little feedback for the developper
          (weka/save-file "/tmp/sila-inua-ML.gnd.arff" ginsts :arff)
          (log/debug (log/<> "GND" sname)
                     (map #(.name ^Attribute %)
                           (enumeration-seq (.enumerateAttributes ginsts))))

          ;; NOTE: We're sending the tweet ID, but the ontology currently ignores it
          ;;
          ;; TODO: Alter the ontology in the DLL
          ;;        MLL            DLL
          ;;         : --tweeter--> :  <alice>
          ;;         : <--counts--- :  <F=f,M=m>
          ;;         : ---gender--> :  <GND=f,F=f+1>
          (>! output {:tweets [sname twid]})
          ; TODO: (<! feedback)

          (recur (<! signal)))))))



;;; --------------------------------------------------------------------------
(defn- go-ML
  "Starts up a machine-learning level and returns its output channel."
  [conns]
  (let [ecfg   (cfg/? :emote)                       ; TODO: use generalized reconfig
        signal (:ml-sig conns)                      ; << instances from Erlang
        emoter (wtw/prep-emoter (TweetToInputLexiconFeatureVector.) ecfg)]

    ;; Start up component layers
    (go-ML-gender conns ecfg)

    ;; Processing loop for the machine-learning layer.
    (go-loop [arff (<! signal)]
      (when-not (= :quit arff)
          (log/info "ML:" arff)
          (let [einsts (doto (weka/load-arff arff)
                             (weka/filter-instances emoter))]
            (log/info "Emotion instances" (.numInstances einsts))
            (doseq [c [:ml-gnd-sig]]  ; TODO: forward emos to other layers...
              (>! (c conns) einsts))
            (recur (<! signal)))))))



;;; --------------------------------------------------------------------------
(defn create!
  "Creates and initializes the hierarchical reasoning architecture."
  []
  ;; Set up the hierarchy...
  (swap! Inua #(if % % (let [conns (connect CONN-KEYS)]
                         (log/notice "Creating the hierarchy of mind")
                         (go-ML conns)
                         (go-DL conns)
                         conns)))

  ;; Return the main input channel (also exported as a convenience)
  (reset! Tell (:ml-sig @Inua)))


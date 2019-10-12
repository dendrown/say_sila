;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Emotion and sentiment analysis Ontology
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.senti
  (:require [say.genie          :refer :all]
            [say.ontology       :refer :all]
            [say.log            :as log]
            [weka.core          :as weka]
            [clojure.core.match :refer [match]]
            [clojure.data.csv   :as csv]
            [clojure.java.io    :as io]
            [clojure.string     :as str]
            [clojure.pprint     :as prt :refer [pp pprint]]
            [tawny.repl         :as repl]                       ; <= DEBUG
            [tawny.owl          :as owl])
  (:import  [weka.core DenseInstance
                       Instances]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const ONT-IRI    "http://www.dendrown.net/uqam/senti.owl#")
(def ^:const ONT-FPATH  "resources/KB/senti.owl")
(def ^:const DATASET    "resources/emo-sa/sentiment-analysis.csv")
(def ^:const ARFF-BASE  "resources/emo-sa/sentiment-analysis.arff")


;;; --------------------------------------------------------------------------
(defn create
  "Initial function to create the senti ontology.  Expect changes."
  ([] (create DATASET))

  ([fpath]
  (let [dsets (atom {})
        base  (weka/load-arff ARFF-BASE)
        rname (.relationName base)
        acnt  (.numAttributes base)
        ->nom #(match % "0" "neg"                       ; To nominal value
                        "1" "pos")

        dset+ (fn [src]
                ; Make new dataset
                (let [insts (doto (Instances. base 0)
                                  (.setRelationName (str rname src)))]
                  (log/info "Adding dataset" src)
                  (swap! dsets assoc src insts)
                  insts))

        data+ (fn [[id pn src txt]]
                ;; Add data Instance; remember, the Weka objects are mutable
                (let [insts (if-let [ds (get @dsets src)] ds (dset+ src))
                      inst  (doto (DenseInstance. acnt)
                                  (.setDataset insts)
                                  (.setValue 0 (Float/parseFloat id))
                                  (.setValue 1 ^String txt)
                                  (.setValue 2 (Float/parseFloat pn)))]
                    (.add ^Instances insts inst)))]

    ;; Separate the sentiment sources
    (with-open [rdr (io/reader fpath)]
      (let [[_ & lines] (csv/read-csv rdr)]

        (loop [ls (take 20 lines)]
          (when-let [line (first ls)]
            (println (count line) ":" line)
            (data+ line)
            (recur (rest ls))))))

    @dsets)))

;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Ontology optimization using Genetic Algorithms
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.optimize
  (:require [say.genie          :refer :all]
            [say.check          :as chk]
            [say.log            :as log]
            [say.senti          :as senti])
  (:import  (io.jenetics BitChromosome
                         Genotype
                         Mutator
                         RouletteWheelSelector
                         SinglePointCrossover)
            (io.jenetics.engine Engine
                                Evaluator
                                EvolutionResult
                                EvolutionStatistics
                                Limits)))

;;; TODO: The say-senti ontology is currently acting as a prototype in the
;;;       Say-Sila project.  The functionality in this module needs to be
;;;       generalized.

(set! *warn-on-reflection* true)

(def ^:const INIT-POPULATION-SIZE   10)
(def ^:const INIT-MUTATION-RATE     0.25)


;;; --------------------------------------------------------------------------
(defn make-evaluator
  "Returns a Jenetics Evaluator to determine the fitness of an individual."
  [arff solns]
  (log/fmt-debug "Evaluate on ~a: cnt[~a]" (type solns) (count solns))
  (reify Evaluator
    (eval [_ geno]
      (let [geno  ^Genotype geno
            ones  (-> (.getChromosome geno)
                      (.as BitChromosome)
                      (.ones))
            trial (reduce #(conj %1 (nth %2 solns)) ones)
            audit (chk/eval-senti arff trial)]

        (log/fmt-info "Evaluating ~a solutions: f1[~4$]" (count ones)
                                                         (.fMeasure audit))
        ;; TODO: Score on F1, not rule count
        (count ones)))))



;;; --------------------------------------------------------------------------
(defn evolve
  "Runs Jenetics GA to determine ideal solutions."
  [arff solns]
  (let [plan    (doto (Engine/builder (make-evaluator arff solns)
                                      (BitChromosome/of (count solns)))
                      (.populationSize INIT-POPULATION-SIZE)
                      (.selector (RouletteWheelSelector.))
                      (.alterers (Mutator. INIT-MUTATION-RATE)
                                 (SinglePointCrossover.)))
        stats   (EvolutionStatistics/ofNumber)
        result  (doto (.build plan)
                      (.stream)
                      (.limit (Limits/bySteadyFitness 5))   ; TODO: config
                      (.limit 100)                          ; TODO: config
                      (.peek stats)
                      (.collect (EvolutionResult/toBestEvolutionResult)))]
    (log/info "Evolution:\n" stats)
    (log/info "Result:\n" result)))



;;; --------------------------------------------------------------------------
(defn judge-senti
  "Uses a genetic algorithm to detemine an ideal subset of solutions for
  identifying positive texts using the say-senti ontology."
  []
  (let [arff    "resources/emo-sa/sentiment-analysis.Sentiment140.r24816.train.000.arff"    ; FIXME!
        solns   (senti/read-solutions)]

    (log/fmt-info "GA selection: arff[~a] solns[~a]" arff (count solns))
    (evolve arff solns)))


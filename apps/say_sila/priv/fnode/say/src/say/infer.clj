;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Ontological reasoning functionality.
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.infer
  (:require [say.genie          :refer :all]
            [tawny.reasoner     :as rsn]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)



;;; --------------------------------------------------------------------------
(defmacro with-silence
  "Performs the reasoning operation in the body using a silent reasoner
  progress monitor."
  [& body]
  `(binding [rsn/*reasoner-progress-monitor* (atom rsn/reasoner-progress-monitor-silent)]
     ~@body))



;;; --------------------------------------------------------------------------
(defn unreason
  "Detaches a reasoner from the specified ontology and destroys it (allowing
  precious memory resources to become available once again."
  ([ont]
  (unreason ont (rsn/reasoner-for-ontology ont)))


  ([ont rsnr]
  ;; Make sure HermiT doesn't hoard memory.  Tawny-OWL (as of version 2.0.3) is
  ;; not calling dispose on the HermiT reasoner due to crashiness they've seen.
  (when rsnr
    (.dispose rsnr)
    (rsn/discard-reasoner ont))))


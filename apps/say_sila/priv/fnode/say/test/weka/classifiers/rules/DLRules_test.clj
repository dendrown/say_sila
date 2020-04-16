(ns weka.classifiers.rules.DLRules-test
  (:require [weka.classifiers.rules.DLRules :refer :all]
            [clojure.test :refer :all]
            [weka.core    :as weka])
  (:import  (weka.classifiers.rules DLRules)
            (weka.core Capabilities$Capability)))


;; ---------------------------------------------------------------------------
(deftest capabilities
  (let [mdl  (DLRules.)
        caps (.getCapabilities mdl)]
    (is (.handles caps Capabilities$Capability/NOMINAL_ATTRIBUTES))))


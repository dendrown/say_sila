(ns say.sila-test
    (:require [clojure.test  :refer :all]
              [say.sila      :refer :all]
              [tawny.owl      :as dl]
              [tawny.reasoner :as rsn]
              [tawny.fixture  :as fxt]))

(use-fixtures :each (fxt/reasoner :hermit))

(deftest reasonable
  (is (rsn/consistent? say-sila))
  (is (rsn/coherent?   say-sila)))

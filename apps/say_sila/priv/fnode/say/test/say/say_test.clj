(ns say.say-test
    (:use [clojure.test])
    (:require
     [say.say :as ont]
     [tawny.owl :as o]
     [tawny.reasoner :as r]
     [tawny.fixture :as f]))

(use-fixtures :each (f/reasoner :hermit))

(deftest reasonable
  (is (r/consistent? say.say/say))
  (is (r/coherent? say.say/say)))

(ns say.senti-test
    (:require [clojure.test   :refer :all]
              [say.senti      :refer :all]
              [tawny.reasoner :as rsn]
              [tawny.fixture  :as fxt]))

(deftest reasonable
  (is (rsn/consistent? say-senti))
  (is (rsn/coherent?   say-senti)))

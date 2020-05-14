(ns say.cmu-pos-test
    (:require [clojure.test   :refer :all]
              [say.cmu-pos    :refer :all]
              [tawny.reasoner :as rsn]
              [tawny.fixture  :as fxt]))

(deftest reasonable
  (is (rsn/consistent? cmu-pos))
  (is (rsn/coherent?   cmu-pos)))

(ns say.senti-test
    (:require [clojure.test   :refer :all]
              [say.senti      :refer :all]
              [say.log        :as log]
              [tawny.reasoner :as rsn]
              [tawny.fixture  :as fxt]))

(def ^:const TEST-DATASET   "resources/test/test.A00.csv")
(def ^:const GOLD-ARFF      "resources/test/test.A00.Sentiment140.GOLD.arff")


;; ---------------------------------------------------------------------------
(deftest reasonable
  (is (rsn/consistent? say-senti))
  (is (rsn/coherent?   say-senti)))


;; ---------------------------------------------------------------------------
(deftest dataset
  (let [{cnt  :count
         arff :fpath} (:Sentiment140 (create-arffs TEST-DATASET))]
    (log/fmt-debug "Created dataset ~a: cnt[~a]" arff cnt)
    (is (= cnt 1))
    (is (= (slurp arff)
           (slurp GOLD-ARFF)))))

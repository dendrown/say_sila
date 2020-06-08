(ns say.senti-test
    (:require [clojure.test   :refer :all]
              [say.senti      :refer :all]
              [say.log        :as log]
              [weka.core      :as weka]
              [tawny.reasoner :as rsn]
              [tawny.fixture  :as fxt]))

(def ^:const TEST-DATASET   "resources/test/test.A00.csv")
(def ^:const GOLD-ARFF      "resources/test/test.A00.Sentiment140.GOLD.arff")

(def ^:const GOLD-EXAMPLE   {:id        29923
                             :polarity  :positive
                             :content   '("@adamlefever" "Sen" "Thai" "let" "me" "down" "when" "I" "took"
                                          "my" "parents" "there" "." "The" "food" "was" "not" "great" "."
                                          "My" "faves" "are" "still" "Manee" "Thai" "and" "Thai" "Kitchen" ".")
                             :pos-tags  '("@" "^" "^" "V" "O" "T" "R" "O" "V" "D" "N" "R" "," "D" "N" "V" "R"
                                          "A" "," "D" "N" "V" "R" "^" "^" "&" "^" "^" ",")
                             :rules     '(#{} #{} #{} #{} #{} #{} #{} #{} #{} #{} #{} #{} #{} #{}
                                          #{"Joy" "Positive" "Trust"} #{} #{"NEGATION"} #{} #{} #{}
                                          #{} #{} #{} #{} #{} #{} #{} #{} #{})})


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


;; ---------------------------------------------------------------------------
(deftest example
  (let [insts (weka/load-arff GOLD-ARFF "sentiment")
        xmps  (instances->examples insts)]
    (is (= (first xmps)
           GOLD-EXAMPLE))))

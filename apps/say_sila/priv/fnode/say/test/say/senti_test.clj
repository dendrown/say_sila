(ns say.senti-test
    (:require [clojure.test     :refer :all]
              [say.senti        :refer :all]
              [say.log          :as log]
              [clojure.data.csv :as csv]
              [clojure.java.io  :as io]
              [weka.core        :as weka]
              [tawny.owl        :as owl]
              [tawny.reasoner   :as rsn]
              [tawny.fixture    :as fxt]))

(def ^:const TEST-DATASET   "resources/test/test.A00.csv")
(def ^:const GOLD-ARFF      "resources/test/test.A00.Sentiment140.GOLD.arff")

(def ^:const GOLD-TWEEBO    "resources/test/tweebo/test.A00.GOLD.twt.predict")
(def ^:const GOLD-ONTOLOGY  "resources/test/tweebo/test.A00.GOLD.owl")
(def ^:const TEST-ONTOLOGY  "resources/test/tweebo/test.A00.owl")

;; NOTE: The Sentiment140 daataset marks t29923 as positive.  Is it really?
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



(defn gold-examples
  []
  (-> (weka/load-arff GOLD-ARFF "sentiment")
      (instances->examples)))


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
    (is (= GOLD-EXAMPLE
           (first (gold-examples)))))


;; ---------------------------------------------------------------------------
(deftest tweebo
  (let [xmps (gold-examples)
        ont  (populate-ontology :lein-test xmps)
        deps (with-open [reader (io/reader GOLD-TWEEBO)]
               (doall (csv/read-csv reader :separator \tab)))]
    (add-dependencies ont (first xmps) deps)
    (owl/save-ontology ont TEST-ONTOLOGY :owl)
    (is (apply = (map slurp [TEST-ONTOLOGY
                             GOLD-ONTOLOGY])))))


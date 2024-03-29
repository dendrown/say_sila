(ns say.senti-test
    (:require [clojure.test     :refer :all]
              [say.senti        :refer :all]
              [say.config       :as cfg]
              [say.log          :as log]
              [clojure.data.csv :as csv]
              [clojure.java.io  :as io]
              [weka.core        :as weka]
              [tawny.owl        :as owl]
              [tawny.reasoner   :as rsn]
              [tawny.fixture    :as fxt]))

(def ^:const TEST-DATASET   "resources/test/test.A00.csv")
(def ^:const GOLD-ARFF      "resources/test/test.A00.Sentiment140.GOLD.arff")

(def ^:const SOLN-OCEL      "resources/test/soln.S00.ocel.txt")
(def ^:const SOLN-CELOE     "resources/test/soln.S00.celoe.txt")

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
(deftest wanted-solutions
  (are [s] (good-solution? s)
    "LearnedPositiveText and (hasComponent some (isPartOfSpeech some Adjective)) (accuracy 60.204%, length 7, depth 2)"
    "hasComponent some (expresses some (Concept and (not (Surprise)))) (accuracy 60.204%, length 8, depth 2)"
    "Text and (hasComponent some (denotesAffect some (not (Negative)))) (pred. acc.: 63.27%, F-measure: 66.04%)"))


;; ---------------------------------------------------------------------------
(deftest unwanted-solutions
  (are [s] (not (good-solution? s))
    "(hasComponent min 5 (follows only (denotesAffect only InformationObject))) (pred. acc.: 56.70%, F-measure: 65.00%)"
    "(Text and (hasComponent some Object)) (accuracy 11.22%, length 5)"
    "(Text and (hasComponent min 5 (follows only (expresses only Nothing))))"))


;; ---------------------------------------------------------------------------
(deftest solutions
  (let [[ocel
         celoe] (map read-solutions [SOLN-OCEL
                                     SOLN-CELOE])]
    (are [x y] (= x y)
     (cap-solutions ocel 1)
     '((tawny.english/and
         (tawny.english/not (tawny.owl/owl-class say.senti/*build-ontology* "LearnedPositiveText-4"))
         (tawny.english/some say.dolce/hasComponent (tawny.english/some say.cmu-pos/isPartOfSpeech say.cmu-pos/CommonNoun))))

      (cap-solutions celoe 1)
      '(say.senti/Text)

      (cap-solutions celoe 2)
      '(say.senti/Text
       (tawny.english/and
         say.senti/Text
         (tawny.english/some say.dolce/hasComponent say.dolce/SocialObject))))))


;; ---------------------------------------------------------------------------
(deftest tweebo
  (if (cfg/?? :senti :use-tweebo?)
    (let [xmps (gold-examples)
          ont  (populate-ontology :lein-test xmps)
          deps (with-open [rdr (io/reader GOLD-TWEEBO)]
                 (doall (csv/read-csv rdr :separator \tab)))]
      (add-dependencies ont (first xmps) deps)
      (owl/save-ontology ont TEST-ONTOLOGY :owl)
      (is (apply = (map slurp [TEST-ONTOLOGY
                               GOLD-ONTOLOGY]))))

    (log/warn "Enable Tweebo for tweebo test")))


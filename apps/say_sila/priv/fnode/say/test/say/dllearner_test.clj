(ns say.dllearner-test
    (:require [clojure.test     :refer :all]
              [say.dllearner    :refer :all]
              [say.log          :as log]
              [say.dolce]
              [say.senti]
              [tawny.english]
              [clojure.string   :as str]))

(def ^:const OCEL-SOLUTIONS "resources/test/soln.S01.ocel.txt")


(defn equiv?
  "Compare DL-Learner rule vs. a test OWL expression."
  [rule owl]
  ;(log/warn "EQ:" rule "=?=" owl)
  (let [eq? #(= rule owl)]
    (cond
      (symbol? rule)          (eq?)                                   ; Same symbols?
      (number? rule)          (eq?)                                   ; Cardinality constant
      (string? rule)          (eq?)                                   ; Used for LearnedPositiveText
      (and (empty? rule)
           (empty? owl))      true                                      ; All done!
      (and (seqable? rule)
           (seqable? owl))    (and (equiv? (first rule) (first owl))    ; Recurse
                                   (equiv? (next  rule) (next  owl)))
      :else                   (boolean                                  ; Bad match!
                              (log/error rule "\n:\n" owl)))))


(defn match?
  "Determines if an OWL solution matches the specified DL-Learner output text."
  [txt owl]
  ;; say.dllearner/read-solution returns a Solution record.
  ;; Here we're just checking the OWL rule
  (equiv? (read-solution txt) owl))


;; ---------------------------------------------------------------------------
(deftest learned-names
  (are [n1 n2] (= n1 n2)
    "LearnedPositiveText"   (name-learned)
    "LearnedPositiveText-9" (name-learned 9)
    "LearnedPositiveText-9" (name-learned "9")))


;; ---------------------------------------------------------------------------
(deftest solution-ordering
  (let [acc   #(:acc (meta (read-solution %)))
        solns (-> (slurp  "resources/test/soln.S01.ocel.txt")
                  (str/split  #"\n")
                  (reorder-solutions :ocel))]
    (are [a which] (= a (acc (which solns)))
      88.888 first
      22.222 last)))


;; ---------------------------------------------------------------------------
(deftest ocel-solutions
  (are [txt owl] (match? txt owl)
    "hasComponent min 3 (precedes max 1 (follows some (denotesAffect some Negative))) (accuracy 66.667%, length 11, depth 4)"
    '(tawny.owl/at-least 3 say.dolce/hasComponent
       (tawny.owl/at-most 1 say.dolce/precedes
         (tawny.english/some say.dolce/follows
           (tawny.english/some say.senti/denotesAffect say.senti/Negative))))

    "LearnedPositiveText-1 and (hasComponent some (denotesAffect some Trust)) (accuracy 65.116%, length 7, depth 2)"
    '(tawny.english/and (tawny.owl/owl-class say.senti/*build-ontology* "LearnedPositiveText-1") (tawny.english/some say.dolce/hasComponent (tawny.english/some say.senti/denotesAffect say.senti/Trust)))))


;; ---------------------------------------------------------------------------
(deftest celoe-solutions
  (are [txt owl] (match? txt owl)
    "Text and (hasComponent some (denotesAffect some ((not (Disgust)) and (not (Negative))))) (pred. acc.: 64.29%, F-measure: 66.67%)"
   '(tawny.english/and
      say.senti/Text
      (tawny.english/some say.dolce/hasComponent
        (tawny.english/some say.senti/denotesAffect
          (tawny.english/and
            (tawny.english/not say.senti/Disgust)
            (tawny.english/not say.senti/Negative)))))

    "Text and (hasComponent some (denotesAffect some (not (Negative)))) (pred. acc.: 63.27%, F-measure: 66.04%)"
    '(tawny.english/and
       say.senti/Text
       (tawny.english/some say.dolce/hasComponent
         (tawny.english/some say.senti/denotesAffect
           (tawny.english/not say.senti/Negative))))

    "Text and (hasComponent some (follows some (denotesAffect some (not (Anticipation))))) (pred. acc.: 60.64%, F-measure: 64.76%)"
    '(tawny.english/and
       say.senti/Text
       (tawny.english/some say.dolce/hasComponent
         (tawny.english/some say.dolce/follows
           (tawny.english/some say.senti/denotesAffect
             (tawny.english/not say.senti/Anticipation)))))

    "Text and (hasComponent some (denotesAffect some (Fear or Joy or Positive))) (pred. acc.: 64.89%, F-measure: 67.33%)"
    '(tawny.english/and
       say.senti/Text
       (tawny.english/some say.dolce/hasComponent
         (tawny.english/some say.senti/denotesAffect
           (tawny.english/or
             say.senti/Fear
             say.senti/Joy
             say.senti/Positive))))))


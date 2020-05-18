(ns say.dllearner-test
    (:require [clojure.test  :refer :all]
              [say.dllearner :refer :all]
              [say.log       :as log]
              [say.dolce]
              [say.senti]
              [tawny.english]))


(defn equiv?
  "Compare DL-Learner rule vs. a test OWL expression."
  [rule owl]
  (cond
    (var? rule)             (= rule (eval owl))                         ; Same symbols?
    (number? rule)          true                                        ; Cardinality constant
    (and (empty? rule)
         (empty? owl))      true                                        ; All done!
    (and (seqable? rule)
         (seqable? owl))    (and (equiv? (first rule) (first owl))      ; Recurse
                                 (equiv? (next  rule) (next  owl)))
    :else                   (boolean                                    ; Bad match!
                              (log/error rule "\n:\n" owl))))


(defn match?
  "Determines if an OWL solution matches the specified DL-Learner output text."
  [txt owl]
  ;; say.dllearner/read-solution returns a Solution record.
  ;; Here we're just checking the OWL rule
  (equiv? (:rule (read-solution txt)) owl))


;; ---------------------------------------------------------------------------
(deftest ocel-solutions
  (are [txt owl] (match? txt owl)
    "hasComponent min 3 (precedes max 1 (follows some (denotesAffect some Negative))) (accuracy 66.667%, length 11, depth 4)"
    '(#'tawny.owl/at-least 3 #'say.dolce/hasComponent
       (#'tawny.owl/at-most 1 #'say.dolce/precedes
         (#'tawny.english/some #'say.dolce/follows
           (#'tawny.english/some #'say.senti/denotesAffect #'say.senti/Negative))))))


;; ---------------------------------------------------------------------------
(deftest celoe-solutions
  (are [txt owl] (match? txt owl)
    "Text and (hasComponent some (denotesAffect some ((not (Disgust)) and (not (Negative))))) (pred. acc.: 64.29%, F-measure: 66.67%)"
   '(#'tawny.english/and
      #'say.senti/Text
      (#'tawny.english/some #'say.dolce/hasComponent
        (#'tawny.english/some #'say.senti/denotesAffect
          (#'tawny.english/and
            (#'tawny.english/not #'say.senti/Disgust)
            (#'tawny.english/not #'say.senti/Negative)))))

    "Text and (hasComponent some (denotesAffect some (not (Negative)))) (pred. acc.: 63.27%, F-measure: 66.04%)"
    '(#'tawny.english/and
       #'say.senti/Text
       (#'tawny.english/some #'say.dolce/hasComponent
         (#'tawny.english/some #'say.senti/denotesAffect
           (#'tawny.english/not #'say.senti/Negative))))

    "Text and (hasComponent some (follows some (denotesAffect some (not (Anticipation))))) (pred. acc.: 60.64%, F-measure: 64.76%)"
    '(#'tawny.english/and
       #'say.senti/Text
       (#'tawny.english/some #'say.dolce/hasComponent
         (#'tawny.english/some #'say.dolce/follows
           (#'tawny.english/some #'say.senti/denotesAffect
             (#'tawny.english/not #'say.senti/Anticipation)))))))


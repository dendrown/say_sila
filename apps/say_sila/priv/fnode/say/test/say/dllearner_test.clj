(ns say.dllearner-test
    (:require [clojure.test  :refer :all]
              [say.dllearner :refer :all]
              [say.log       :as log]
              [say.dolce]
              [say.senti]
              [tawny.english]))


(defn equiv?
  "Compare DL-Learner vs. the test OWL"
  [soln owl]
  (cond
    (var? soln)             (= soln (eval owl))                         ; Same symbols?
    (and (empty? soln)
         (empty? owl))      true                                        ; All done!
    (and (seqable? soln)
         (seqable? owl))    (and (equiv? (first soln) (first owl))      ; Recurse
                                 (equiv? (next  soln) (next  owl)))
    :else                   (boolean                                    ; Bad match!
                              (log/error soln "\n:\n" owl))))



;; ---------------------------------------------------------------------------
(deftest read-solutions
  (run! (fn [[txt owl]]
          (let [soln (:soln (read-solution txt))]
            (is (equiv? soln owl))))
        [["Text and (hasComponent some (denotesAffect some (not (Negative)))) (pred. acc.: 63.27%, F-measure: 66.04%)"
          '(#'tawny.english/and
             #'say.senti/Text
             (#'tawny.english/some #'say.dolce/hasComponent
               (#'tawny.english/some #'say.senti/denotesAffect
                                     (#'tawny.english/not #'say.senti/Negative))))]]))

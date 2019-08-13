(ns weka.filters.unsupervised.attribute.TweetToGenderFeatures-test
  (:require [weka.filters.unsupervised.attribute.TweetToGenderFeatures :refer :all]
            [clojure.test :refer :all]
            [say.weka     :as weka])
  (:import  [weka.filters.unsupervised.attribute TweetToGenderFeatures]))

(def ^:const ARFF-FPATH "resources/test/gender.test.arff")

(defn setup-filter []
    (doto (TweetToGenderFeatures.)
          (.setScreenNameIndex  "3")
          (.setFullNameIndex    "4")
          (.setDescriptionIndex "5")
          (.setTextIndex        "6")))


;; ---------------------------------------------------------------------------
(deftest options
  (let [sieve  (setup-filter)]
    (are [x y] (= x y) 
      "3" (.getScreenNameIndex   sieve)
      "4" (.getFullNameIndex     sieve)
      "5" (.getDescriptionIndex  sieve)
      "6" (.getTextIndex         sieve))))


;; ---------------------------------------------------------------------------
(deftest get-options
  (is (= '("-D" "5" "-N" "4" "-S" "3") (take 6 (seq (.getOptions (setup-filter)))))))


;; ---------------------------------------------------------------------------
(deftest filtering
  (let [sieve    (setup-filter)
        insts    (weka/load-arff ARFF-FPATH)
        in-cnt   (.numAttributes insts)
        out-cnt  (* 2 (count ["S" "N" "D"]))            ; F&M attr for each option
        result   (weka/filter-instances insts sieve [])
        [male
         female] (map #(.instance result %) [0 1])]

    (are [x y] (= x y)
      (.numAttributes result) (+ in-cnt
                                 out-cnt)
      '(0 1
        0 2
        0 3) (map #(int (.value male (+ in-cnt %))) (range out-cnt))
      '(3 0
        2 0
        1 0) (map #(int (.value female (+ in-cnt %))) (range out-cnt)))))


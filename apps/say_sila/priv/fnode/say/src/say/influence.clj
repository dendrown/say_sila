;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Code for studies on influence within social media networks
;;;;
;;;; @copyright 2018 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.influence
  (:require [say.log        :as log]
            [say.weka       :as weka]
            [clojure.string :as str]
            [incanter.core  :refer :all]
            [incanter.io    :refer :all]
            [incanter.stats :refer :all])
  (:import  [weka.core Instances]))


(set! *warn-on-reflection* true)

(def ^:const TOP-N      12)
(def ^:const EMOTIONS   [:anger :fear :sadness :joy])
(def ^:const FMT-CSV    "/srv/say_sila/weka/gw_full_nlp_n5-25_oter_~a_NN_oter_~a_n~a.csv")


;;; --------------------------------------------------------------------------
(defn attr-stats
  "
  Run generation scripts for influence article.
  "
  ([]
  (doseq [emo EMOTIONS]
    (attr-stats emo)))


  ([emotion]
  (let [emo   (name emotion)
        arff  (log/fmt "/srv/say_sila/weka/gw_full_nlp_n5-25_oter_~a_oter_~a.arff" emo emo)
        insts (weka/load-arff arff)]

    ;; Skip first DTS attribute (minute)
    (log/fmt! "~8a" emo)
    (doseq [ai (range 1 (.numAttributes insts))]
      ;; Only do attributes for this emotion
      (when (= emo
              (last (str/split (.name (.attribute insts (int ai))) #"_" 3)))
        (let [stats (.numericStats (.attributeStats insts ai))]
          (log/fmt! " & ~3$ & ~3$" (.mean   stats)
                                 (.stdDev stats)))))
    (println " \\\\"))))



;;; --------------------------------------------------------------------------
(defn read-data
  "
  Run generation scripts for influence article.
  "
  ([emotion]
  (read-data emotion TOP-N))

  ([emotion n]
  (let [emo  (name emotion)
        csv  (log/fmt FMT-CSV emo emo n)
        data (read-dataset csv :header true)
        cols (col-names data)
        pred (last cols)
        dcol (dec (count cols))
        dep  (sel data :cols dcol)
        inds (sel data :cols (range dcol))]

    (log/info "Reading data:" csv)
    (log/info "Prediction  :" (name pred))
    (log/info "Independents:" (map name (col-names inds)))
    {:pred  pred
     :dep   dep
     :inds  inds
     :n     n})))



;;; --------------------------------------------------------------------------
(defn model-nn
  "
  Creates an OLS linear regression model and turns it into a LaTeX table line.
  "
  ([]
  (let [dsets (map #(read-data :anger %) (range 5 26))
        inds  (reduce #(distinct (concat %2 %1))
                       (map #(col-names (:inds %)) dsets))]

    ;; TODO: Find the union of coefficients
    (log/fmt! "N  &  PCC ")
    (doseq [ind inds]
      (log/fmt!" & ~a" (name ind)))

    (log/fmt! "~%")
    (log/fmt! "\\midrule %--------------------------------------------------------------------~%")
    (doseq [ds dsets]
    (model-nn ds))))


  ([{:keys [pred dep inds n]}]
  (let [lmod            (linear-model dep inds)
        [intc & coefs]  (:coefs    lmod)
        anames          (col-names inds)]

    ;; Start with all the multirow values
    (log/fmt! "~2@a &  0.PCC" n)
    (doseq [coef coefs]
      (log/fmt! " & ~3$" coef))

    (log/fmt! " & ~3$ \\\\~%" intc))))



;;; --------------------------------------------------------------------------
(defn model
  "
  Creates an OLS linear regression model and evaluates it.
  "
  [{:keys [pred dep inds]}]
  (let [lmod                 (linear-model dep inds)
        [regs comm emo]      (str/split (name pred) #"_" 3)  
        [intc    & coefs]    (:coefs    lmod)
        [intc-t  & coefs-t]  (:t-probs  lmod)
        [intc-ci & coefs-ci] (:coefs-ci lmod)
        anames               (col-names inds)
        acnt                 (inc (count anames))
        places               "&&&"]

    (letfn [;; ---------------------------------------------------------------
            (attribber [a]
              (str/join "\\_" (str/split (name a) #"_")))

            ;; ---------------------------------------------------------------
            (multirow [x]
              (log/fmt "\\multirow{~a}{*}{~a}" acnt x))

            ;; ---------------------------------------------------------------
            (numeric ([chk]     (numeric chk "~4$"))
                     ([chk fmt] (log/fmt fmt (lmod chk))))]

    ;(log/fmt-notice "Model for ~a:" emo)
    (println "\\midrule %--------------------------------------------------------------------")

    ;; Start with all the multirow values
    (apply log/fmt! "~a &\n~a &\n~a\n" (map multirow [emo
                                                      (second (:df lmod))
                                                      (numeric :r-square)]))
    ;; Finish that line with the first attribute
    (log/fmt! "   & ~a & ~4$ & ~4$ & ~4$ -- ~4$ \\\\\n" (attribber (first anames)) 
                                                        (first  coefs)
                                                        (first  coefs-t)
                                                        (first  (first coefs-ci))
                                                        (second (first coefs-ci)))
    (println "%----")

    ;; Add the single liners for this model
    (doseq [values (map vector (map attribber (rest anames))
                               (rest coefs)
                               (rest coefs-t)
                               (rest coefs-ci))]

      (apply (fn [attr coef p [lo hi]] (log/fmt! "~a ~a & ~4$ & ~4$ & ~4$ -- ~4$ \\\\\n%----\n"
                                                 places attr coef p lo hi))
             values))

    ; Incanter likes the intercept first, but we want it last
    (log/fmt! "~a intercept & ~4$ & ~4$ & ~4$ -- ~4$ \\\\\n"
              places intc intc-t (first intc-ci) (second intc-ci))
    (println))))


;;; --------------------------------------------------------------------------
(defn start
  "
  Run generation scripts for influence article.
  "
  []
  (doseq [emo EMOTIONS] (model (read-data emo))))


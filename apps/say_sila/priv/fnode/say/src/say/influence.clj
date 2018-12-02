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
            [say.genie      :refer :all]
            [incanter.core  :refer :all]
            [incanter.io    :refer :all]
            [incanter.stats :refer :all])
  (:import  [weka.core Instances]))


(set! *warn-on-reflection* true)

(def ^:const TOP-N       12)
(def ^:const FIRST-N     5)
(def ^:const LAST-N      25)
(def ^:const PLAYERS     [:big   :reg])
(def ^:const COMM-CODES  [:oter  :rter :rted    :tmed])
(def ^:const EMOTIONS    [:anger :fear :sadness :joy])
(def ^:const MIDRULE     "\\midrule %--------------------------------------------------------------------")
(def ^:const FMT-CSV     "/srv/say_sila/weka/gw_full_nlp_n5-25_oter_~a_NN_oter_~a_n~a.csv")
(def ^:const FMT-ERL-CSV "/srv/say_sila/influence/gw_full_nlp_n5-25_oter_~a_NN.csv")


;;; --------------------------------------------------------------------------
(defn cmp-attrs
  "
  Compares two attributes for ordering.
  "
  [attr1 attr2]
  ;; Pair up the parts of the attributes
  (loop [parts   (apply #(zip %1 %2) (map #(str/split (name %) #"_")  [attr1 attr2]))
         cmps    [PLAYERS COMM-CODES EMOTIONS]]

    (if (seq? parts)
      ;;
      ;; Figure out where the parts are in the comparator sequences
      (let [cmp  ^clojure.lang.PersistentVector (first cmps)
            ndxs (map #(.indexOf cmp (keyword %)) (first parts))]
        (cond
          (apply > ndxs) +1
          (apply < ndxs) -1
          :else          (recur (next parts) (next cmps))))

      ;; Nothing more to compare...they were equal!
      0)))



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
        dcol (dec (count cols))]

    ;; Make sure there are independents to process
    (if (pos? dcol)
      (let [dep  (sel data :cols dcol)
            inds (sel data :cols (range dcol))]

        (log/info "Reading data:" csv)
        (log/info "Prediction  :" (name pred))
        (log/info "Independents:" (map name (col-names inds)))
        {:pred  pred
         :dep   dep
         :inds  inds
         :N     n})

      (do (log/warn "No independent attributes in" csv)
          nil)))))



;;; --------------------------------------------------------------------------
(defn model-nn
  "
  Creates an OLS linear regression model and turns it into a LaTeX table line.
  "
  ;;; Run through all emotions
  ([]
  (model-nn FIRST-N))


  ;;; Accept emotion keyword or a starting value for N
  ([arg]
  (cond
    (keyword? arg) (model-nn arg FIRST-N)
    (number?  arg) (doseq [emo EMOTIONS] (model-nn emo arg))
    :else          (log/error "Bad argument")))


  ;;; Report on an emotion across a range of values for N
  ([emo first-N]
  (let [dsets   (map #(read-data emo %) (range first-N (inc LAST-N)))
        cols    (sort cmp-attrs
                      (reduce #(distinct (concat %1 %2))
                                         (map #(col-names (:inds %)) dsets)))
        erl-csv (log/fmt FMT-ERL-CSV (name emo))
        erl-out (read-dataset erl-csv :header true)
        pccs    (drop (- first-N
                         FIRST-N) ($ :Correlation erl-out))     ; Skip PCCs for skipped N values
        pcc!    (reduce #(if (> %1 %2) %1 %2) pccs)]            ; Best of all PCCs

    ;; Let logging finish up, then do the table heading
    (log/fmt-notice "LaTeX~a: inf[~a]" emo erl-csv)
    (log/wait)
    (println)
    (println)
    (log/fmt! "N & PCC")
    (doseq [col cols]
      (log/fmt!" & ~a" (name col)))

    (log/fmt! " & intercept \\\\~%~a~%" MIDRULE)

    ;; Check all the models and output the statistics
    (doseq [[n rpt] (doall (map (fn [[dset pcc]] (model-nn dset cols pcc pcc!))
                            (zip dsets pccs)))]
      ;; TODO: Just print for TOP-N
      (log/fmt! "~an=~2@a: ~a~%" (if (= n TOP-N) "*" " ") n rpt))))


  ;;; Report on an emotion for a specific value for N
  ([{:keys [dep inds N]} all-cols pcc pcc!]
  (let [lmod            (linear-model dep inds)
        [intc & coefs]  (:coefs lmod)
        [df1    df2]    (:df    lmod)
        [fstat
         fprob
         adj-r2]        (map #(lmod %) [:f-stat :f-prob :adj-r-square])
        cols            (col-names inds)
        fmt-fprob      #(cond (<= fprob 0.01) "$^{**}$      "
                              (<= fprob 0.05) "$^{*\\ \\ \\!}$ "
                              :else           "$^{\\ \\ \\ \\!}$")
        fmt-pcc        #(log/fmt (if (= pcc pcc!)
                                     "\\textbf{~3,,6$}"
                                     "        ~3,,6$ ") pcc)
        liner          #(when (= N TOP-N)
                          (log/fmt! "~a~%" MIDRULE))]

    ;; Start with all the multirow values
    (liner)
    (log/fmt! "~2@a ~a & ~a" N (fmt-fprob) (fmt-pcc))
    (loop [mod-cols  (map #(some #{%} cols) all-cols)
           mod-coefs coefs]
      ;; Process once per column in the complete column list
      (when mod-cols
        (if (first mod-cols)
            ;;
            ;; Model has the column, print its coefficient
            (do (log/fmt! " & ~3,,6$" (first mod-coefs))
                (recur (next mod-cols) (next mod-coefs)))

            ;; Model doesn't use this coefficient, print a placeholder
            (do (log/fmt! " &       ")
                (recur (next mod-cols) mod-coefs)))))

    (log/fmt! " & ~3,,6$ \\\\~%" intc)
    (liner)

    ;; Prepare the model's statistical description
    [N (log/fmt "$F(~a,~a) = ~3,,6$, p < ~,2,2e R_{adj}^{2} = ~3$"
                df1 df2 fstat fprob adj-r2)])))



;;; --------------------------------------------------------------------------
(defn model
  "
  Creates an OLS linear regression model and evaluates it.
  "
  ([]
  (doseq [emo EMOTIONS] (model (read-data emo))))


  ([{:keys [pred dep inds]}]
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
            (numeric ([chk]     (numeric chk "~3$"))
                     ([chk fmt] (log/fmt fmt (lmod chk))))]

    ;(log/fmt-notice "Model for ~a:" emo)
    (println "\\midrule %--------------------------------------------------------------------")

    ;; Start with all the multirow values
    (apply log/fmt! "~a &\n~a &\n~a\n" (map multirow [emo
                                                      (second (:df lmod))
                                                      (numeric :r-square)]))
    ;; Finish that line with the first attribute
    (log/fmt! "   & ~a & ~3$ & ~3$ & ~3$ -- ~3$ \\\\\n" (attribber (first anames))
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

      (apply (fn [attr coef p [lo hi]] (log/fmt! "~a ~a & ~3$ & ~3$ & ~3$ -- ~3$ \\\\\n%----\n"
                                                 places attr coef p lo hi))
             values))

    ; Incanter likes the intercept first, but we want it last
    (log/fmt! "~a intercept & ~3$ & ~3$ & ~3$ -- ~3$ \\\\\n"
              places intc intc-t (first intc-ci) (second intc-ci))
    (println)))))


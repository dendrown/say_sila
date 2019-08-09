;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Data collection/preprocessing routings
;;;;
;;;; @copyright 2019 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.data
  (:require [say.genie              :refer :all]
            [clojure.string         :as str]
            [net.cgrand.enlive-html :as web])
  (:import  [java.net URL]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const NAMES  {:female {:url "http://www.20000-names.com/female_english_names" :cnt 20}
                     :male   {:url "http://www.20000-names.com/male_english_names"   :cnt 17}
                     :fpath  "resources/gender/names.edn"})


;;; --------------------------------------------------------------------------
(defn get-dom
  "Pull the document object model from a web resource."
  [url]
  (-> url URL. web/html-resource))


;;; --------------------------------------------------------------------------
(defn get-content
  "Pulls the :content value from a web structure."
  ([x]
  (first (:content x))))



;;; --------------------------------------------------------------------------
(defn get-content!
  "Pulls the innermost :content value from a nested web structure."
  ([xx]
  (loop [x xx]
    (when-let [c (get-content x)]
      (if (string? c)
           (str/trim c)
           (recur c))))))



;;; --------------------------------------------------------------------------
(defn get-names
  "Pull English male and female names from the http://www.20000-names.com
  Note that the <ol> lists on the page seem a bit funky, and so we're
  pulling the names by the colour of the font!"
  ([]
  ;; Pull ALL the names
  (into {}
    (for [[gender {:keys [url cnt]}] NAMES]
      [gender
       (reduce #(into %1 (get-names (str url
                                         (if (< %2 2) "" (strfmt "_~2,'0d" %2))
                                         ".htm")))
               #{} (range 1 (inc cnt)))])))


  ([url]
  (let [COLOR "#9393FF"
        items (-> url get-dom
                  (web/select [:body [:font (web/attr? :color)]]))
        names (filter #(= (get-in % [:attrs :color]) COLOR) items)]

    ;; Don't look like a bot!
    (Thread/sleep (rand-int 20000))

    ;; We're got some clean-up to do...
    (println "Fetching names from:" url)
    (set (filter seq (map get-content! names))))))




;;; --------------------------------------------------------------------------
(defn save-names
  "Saves a hash-map of :female and :male names for later use."
  ([names]
  (save-names names (:fpath NAMES)))

  ([names fpath]
  (let [finalize  (fn [nset]
                    (into (sorted-set) (map #(str/lower-case %) nset)))
        finalists (reduce (fn [acc [gnd nset]]
                            (assoc acc gnd (finalize nset)))
                          {}
                          names)]
    (println "Saving names to" fpath
             (map (fn [[g nn]] {g (count nn)}) finalists))      ; Report counts
    (spit fpath (pr-str finalists)))))


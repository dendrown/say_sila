;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Climate survey modelling (i.e., the Six Americas)
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.survey
  (:require [say.genie          :refer :all]
            [say.config         :as cfg]
            [say.log            :as log]
            [say.social         :as soc]
            [say.wordnet        :as word]
            [weka.tweet         :as tw]
            [clojure.set        :as set]
            [clojure.string     :as str]
            [clojure.pprint     :refer [pp]]
            [incanter.core      :refer [dataset view with-data $where]]
            [incanter.charts    :refer [bar-chart]])
  (:import  [weka.core.stemmers SnowballStemmer]))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Init-Survey    :sassy)


;;; Handle words that are usually run together without camelOrPascalCase (generally to make a hashtag)
(defonce Word-Splits {;; NOTE: We're not including the hashtags used for tweet collection
                      ;.       as all the tweets will have those token sets
                      ;"climatechange"   ["climate" "change"]
                      ;"globalwarming"   ["global" "warming"]
                     })

(defonce Mention-Words          {"Thirdperson"       #{"he" "she" "they"}
                                 "Reference"         #{;speak_of             ; TODO: bigrams
                                                       "believe" "prefer"
                                                       "protect" "support"
                                                       "push"}})

;;; Six Americas Surveys:
;;;
;;; Word sets which invoke essential concepts
(def ^:const Concept-Triggers   {;; ------------------------------------------
                                 "CAUSE"        #{"cause"}
                                 "HUMAN"        #{"human"}
                                 "NATURE"       #{"nature"}
                                 ;; ------------------------------------------
                                 "ENERGY"       #{"energy"}
                                 "CONSERVATION" #{"conservation" "save"}
                                 ;; ------------------------------------------
                                 "CUT"          #{"cut" "regulate"}
                                 "CO2"          #{"co2" "carbon"}
                                 ;; ------------------------------------------
                                 "PROTECT"      #{"protect"}
                                 "ENVIRONMENT"  #{"environment"}
                                 ;; ------------------------------------------
                                 "ECONOMIC"     #{"economic"}
                                 "GROWTH"       #{"growth"}
                                 ;; ------------------------------------------
                                 "PEOPLE"       #{"people"}
                                 "HARM"         #{"harm"}
                                 "YEAR"         #{"year"}
                                 ;; ------------------------------------------
                                 ;; TODO: Remove trials with low coverage
                                 ;; ------------------------------------------
                                 "COMPANY"      #{"company"}
                                 "REWARD"       #{"reward"}
                                 "PUNISH"       #{"punish"}
                                })

(defonce Concept-Words          (word/synonym-values Concept-Triggers))             ; Synonym expansion

(defonce Concept-Stems          (update-values Concept-Words #(tw/stem-all % :set)))


;;; TODO for keywords
;;; - Model Key-Words as #{general alarmed ... dismissive}
;;; - handle bigrams & trigrams

(defonce Question-Words {:t2  #{"change" "mind" "global" "warming" "issues"             ; Table 2
                                "information" "opinion"}

                         :t3  #{"experts" "global" "warming" "really" "happening"       ; Table 3
                                "cause" "harm" "United" "States" "reduce"}

                         :t4  #{"global" "warming" "conserving" "energy"}               ; Table 4

                         :t5  #{"caused" "human" "activities" "natural"                 ; Table 5
                                "changes" "environment" "happening"
                                "scientists" "global" "warming"
                                "disagreement" "informed"}

                         :t6  #{"interested" "disgusted" "helpless" "hopeful"           ; Table 6
                                "sad" "angry" "afraid" "guilty" "depressed"}

                         :t7  #{"important" "issue" "global" "warming" "today"          ; Table 7
                                "worried"}

                         :t8a #{"global" "warming" "harm" "personally" "family"         ; Table 8a
                                "community" "people" "United" "States"
                                "modern" "industrialized" "countries" "developing"
                                "future" "generations" "plant" "animal" "species"}

                         :t8b #{"global" "warming" "start" "harm" "people" "United"     ; Table 8b
                                "States" "now" "years" "never" "world"}

                         :t9  #{"personally" "experienced" "effects" "global"           ; Table 9
                                "warming" "record" "snowstorms" "winter" "eastern"
                                "United" "States" "question" "occurring" "local"
                                "area" "snow" "rain" "different" "normal" "warmer"
                                "colder"}

                         :t10 #{"economic" "downturn" "country" "hurt" "family"         ; Table 10
                                "economy" "bad" "shape" "US" "afford" "reduce"
                                "global" "warming"}

                         :t11 #{"protecting" "environment" "economic" "growth"          ; Table 11
                                "provides" "jobs" "reduces" "conflict"
                                "environmental" "protection" "important" "problems"}

                         :t12 #{; BIGRAMS: "turn off" "air conditioning"                ; Tables 12,13,17,18
                                ;          "brush teeth" "wash dishes"
                                ;          "power strips" "surge protectors"
                                ;          "light bulbs" "compact fluorescent"
                                ;          "air conditioning" "beverage container"
                                ;          "public transportation"
                                "lights" "electronics" "TVs" "computers" "recycle"
                                "home" "winter" "thermostat" "degrees" "cooler"
                                "summer" "warmer" "air" "conditioning" "re-usable"
                                "beverage" "container" "reduce" "trash" "garbage"
                                "water" "shower" "brush" "teeth" "wash" "dishes"
                                "unplug" "power" "strips" "surge" "protectors"
                                "walk" "bike" "driving" "public" "transportation"
                                "carpool" "light" "bulbs" "energy-efficient"
                                "compact" "fluorescent" "CFLs"}

                         :t14 #{; BIGRAMS: "United States"                              ; Table 14
                                "energy-saving" "actions" "months" "reduce"
                                "personal" "contribution" "global" "warming"
                                "United" "States" "people" "modern" "industrialized"
                                "countries" "world"}

                         :t15 #{; BIGRAMS: "taking steps"                               ; Table 15
                                "rewarde" "companies" "steps" "reduce" "global"
                                "warming" "buy" "products" "punish" "opposing"}

                         :t16 #{; BIGRAMS:                                              ; Table 16
                                "volunteere" "donate" "money" "organization"
                                "working" "reduce" "global" "warming" "posted"
                                "comment" "online" "response" "news" "story" "blog"
                                "written" "letters" "emailed" "phoned" "government"
                                "officials" "contacted" "urged" "take" "action"
                                "volunteer"}

                         :t19 #{; BIGRAMS:                                              ; Table 19
                                "home" "family" "friends" "people" "global" "warming"
                                "spoken" "share" "views"}

                         :t20 #{; BIGRAMS: "saving energy"                              ; Table 20
                                "discuss" "global" "warming" "children" "agree"
                                "saving" "energy"}

                         :t21 #{; BIGRAMS:                                              ; Table 21
                                "people" "spoken" "global" "warming" "information"
                                "advice"}

                         :t22 #{; BIGRAMS:                                              ; Table 22
                                "view" "humans" "reduce" "global" "warming"
                                "successfully" "unclear" "change" "behavior"
                                "happening" "actions" "single" "individual"
                                "difference"}

                         :t23 #{; BIGRAMS: "United States"                              ; Table 23
                                "countries" "industrialized" "England" "Germany"
                                "Japan" "reduce" "emissions" "developing" "China"
                                "India" "Brazil" "US" "United" "States" "large-scale"
                                "effort" "large" "economic" "costs" "medium-scale"
                                "moderate" "small-scale" "small"}

                         :t24 #{; BIGRAMS:                                              ; Table 24
                                "low" "medium" "high" "priority" "president"
                                "congress" "developing" "sources" "clean" "energy"}

                         :t25 #{; BIGRAMS : "tax rebates" "solar panels"                ; Table 25
                                ;            "carbon dioxide" "greenhouse gas"
                                ;            "offshore drilling" "natural gas"
                                ;            "renewable energy" "energy sources"
                                ;            "United States" ""nuclear power"
                                ;            "power plants" energy efficient"
                                ;            "electric bill" "income tax"
                                ;            "average household" international treaty"
                                ; TRIGRAMS: "renewable energy sources"
                                "support" "oppose" "policies" "fund" "research"
                                "renewable" "energy" "sources" "solar" "wind" "power"
                                "provide" "tax" "rebates" "people" "purchase"
                                "energy-efficient" "vehicles" "panels" "regulate"
                                "carbon" "dioxide" "primary" "greenhouse" "gas"
                                "pollutant" "expand" "offshore" "drilling" "oil"
                                "natural" "U.S." "coast" "Require" "electric"
                                "utilities" "produce" "electricity" "cost" "average"
                                "household" "sign" "international" "treaty" "requires"
                                "United" "States" " emissions" "build" "nuclear"
                                "plants" "establish" "special" "help" "buildings"
                                "efficient" "teach" "Americans" "reduce" "surcharge"
                                "bill" "increase" "taxes" "gasoline" "cents" "gallon"
                                "return" "revenues" "taxpayers" "reducing" "federal"
                                "income"}

                         :t26 #{; BIGRAMS: "news organizations"                          ; Table 26,27
                                "news" "controversial" "emails" "climate" "scientists"
                                "England" "US" "organizations" "release" "Climategate"
                                "followed" "stories" "closely" "certain" "global" "warming"
                                "happening" "influence" "trust" "errors" "Intergovernmental"
                                "Panel" "Climate" "Change" "IPCC" "report" "certainty"}

                         :t28 #{; BIGRAMS : "mainstream news" "religious leaders"        ; Table 28
                                ; TRIGRAMS: "mainstream news media"
                                "trust" "distrust" "source" "information" "global"
                                "warming" "scientists" "mainstream" "news" "media"
                                "television" "weather" "reporters" "religious" "leaders"}

                         :t29 #{; BIGRAMS : "media sources" "world events"               ; Table 29
                                ; TRIGRAMS: "point of view"
                                "media" "sources" "current" "news" "world" "events"
                                "television" "internet" "email" "radio" "print"
                                "newspapers" "magazines" "political" "point" "view"
                                "share"}

                         :t30 #{; BIGRAMS : "TV news" "Weather Channel" "Fox News"       ; Table 30
                                ;           "OReilly Factor" "Bill OReilly" "Glenn Beck"
                                ;           "Daily Show" "Jon Stewart" "Sean Hannity"
                                ;           "Rush Limbaugh" "Keith Olbermann"
                                ;           "Rachel Maddow" "Colbert Report"
                                ;           "Stephen Colbert"
                                ; TRIGRAMS: "Meet the Press" "New York Times"
                                ;           "Wall Street Journal"
                                "watch" "listen" "shows" "visit" "websites" "local"
                                "TV" "news" "national" "nightly" "network" "CBS" "ABC"
                                "NBC" "newspaper" "print" "online" "Weather" "Channel"
                                "Fox" "cable" "CNN" "Public" "Radio" "NPR" "Sunday"
                                "morning" "Meet" "Press" "MSNBC" "OReilly" "Factor" "Bill"
                                "Glenn" "Beck" "Program" "Daily" "Show" "Jon" "Stewart"
                                "Sean" "Hannity" "Rush" "Limbaugh" "New" "York" "Times"
                                "Countdown" "Keith" "Olbermann" "Rachel" "Maddow"
                                "Colbert" "Report" "Stephen" "Wall" "Street" "Journal"}

                         :t31 #{; BIGRAMS : "Tea Party"                                  ; Table 31
                                ; TRIGRAMS: "middle of the road"
                                "Republican" "Democrat" "Independent" "party" "interested"
                                "politics" "liberal" "moderate" "conservative" "Tea" "Party"
                                "movement" "registered" "vote"}})

(defonce Question-Stems (update-values Question-Words #(tw/stem-all % :set)))

(defonce Key-Words  {:six36 (apply set/union (vals Question-Words))
                     :sassy #{"think"
                              "global" "warming"
                              "harm"
                              "future" "generations" "people"
                              "important" "issue" "personally"
                              "worried"}

                     :wild  #{}})


(defonce Stem-Words     (update-values Key-Words #(tw/stem-all % :set)))

(defonce Stem-Counts    (agent (into {} (map #(vector % {}) (keys Stem-Words)))))


;;; --------------------------------------------------------------------------
(defn which-survey
  "Returns the name of the active (configured) survey."
  []
  (cfg/?? :survey :survey Init-Survey))



;;; --------------------------------------------------------------------------
(defn stem
  "Uses the snowball stemmer to reduce a word to its grammatical stem."
  ([word]
  (stem (tw/make-stemmer) word))


  ([^SnowballStemmer sball word]
  (.stem sball word)))



;;; --------------------------------------------------------------------------
(defn in-stems?
  "Returns true if the token is a keyword in the specified collection of stem
  hits.  The function allows the specification of an aggregation check to
  require that all word parts (every? [default]) be hits or if only some of
  them need to be."
  ([hits token sball]
  (in-stems? hits token sball every?))


  ([hits token sball aggfn]
  (let [word    (soc/unhashtag token)
        check   #(some #{(stem sball %)} hits)
        recheck #(and %                         ; Make sure the lookup succeeded
                      (at-least? 2 %)           ; One means the original word
                      (aggfn check %))]         ; All (or some) parts must be hits

    (boolean (or (check word)                                    ; 1: Basic token check
                 (recheck (Word-Splits word))                    ; 2: Hashtag values (w/out #)
                 (recheck (soc/tokenize word :lower-case)))))))  ; 3: Hyphens, WeirdCase, etc.



;;; --------------------------------------------------------------------------
(defn in-survey?
  "Returns true if the specified word is a keyword in the indicated survey."
  ([survey word]
  (in-survey? survey word (tw/make-stemmer)))


  ([survey word sball]
  (let [hits  (Stem-Words survey)
        in?   (or (= :wild survey)
                  (in-stems? hits word sball))]

    ;; If we're being wild, update for all tokens even as we process another survey
    (when (and (not= :wild survey)
               (= :wild (cfg/?? :survey :survey)))
      (in-survey? :wild word sball))

    ;; Update our survey "hit" reporting
    (when in?
      ;; The agent will create its own stemmer so it doesn't stomp ours!
      (send Stem-Counts #(update-in % [survey (stem word)] (fnil inc 0))))
    in?)))



;;; --------------------------------------------------------------------------
(defn conceptualize
  "Returns a concept key if the word indicates a survey-defined concept;
  otherwise, returns nil."
  ([word]
  (conceptualize word (tw/make-stemmer)))


  ([word sball]
  (let [check    (fn [hits tok]
                    (in-stems? hits tok sball))
        conceive (fn [tok]
                   ;; Grab the true keys from a converted map {concept true|false}
                   (keys (filter second
                                (update-values Concept-Stems #(check % tok)))))
        tokens   (soc/tokenize word :lower-case)]
    (into #{} (mapcat conceive tokens)))))



;;; --------------------------------------------------------------------------
(defn report-survey
  "Reports word (stem) coverage for words checked against surveys associated
  with this namespace."
  ([]
  (run! report-survey (keys Stem-Words)))


  ([survey & opts]
  (await Stem-Counts)
  (let [hits  (cfg/?? :survey :min-chart-hits 5)
        stems (sort (fn [[_ a] [_ b]]
                      (> a b))
                    (@Stem-Counts survey))]

    (when (some #{:chart} opts)
      (future
       (with-data (->> stems
                       (dataset [:stem :freq])
                       ($where {:freq {:gte hits}}))
         (view (bar-chart :stem :freq
                          :title (strfmt "~a Keyword Hits (min ~a)" (KEYSTR survey) hits)
                          :y-label "frequency"
                          :x-label "keyword stem"       ; x/y-labels are flipped for non-vertical chart
                          :vertical false)))))

    (run! (fn [[stem freq]]
            (when (>= freq hits)
              (log/fmt-info "~16a: ~4a" stem freq)))
          stems))))


;;; --------------------------------------------------------------------------
(defn count-affect
  "Returns the counts of affect hits in keywords from the full :six36 survey."
  ([]
  (count-affect :six36))

  ([survey]
  (let  [lex (tw/make-lexicon (cfg/?? :sila :lexicon :nrc))

         ;; Affect hits by word
         ss  (reduce (fn [acc w]
                       (assoc acc w (tw/analyze-token+- lex w tw/Affect-Namer)))
                     {}
                     (Key-Words :six36))

         ;; Remove empties
         saff (select-keys ss (for [[k v :as kv] ss :when (not-empty v)] k))]
    ;; Return single map of counts, keyed by affect terms
    (reduce (fn [acc vs]
              (merge-with + acc (into {} (zip vs (repeat 1)))))
            {}
            (vals saff)))))


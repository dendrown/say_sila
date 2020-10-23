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

;;; Six Americas Surveys:
;;;
;;; Word sets which invoke essential concepts
(def ^:const Concept-Triggers   {;; ------------------------------------------
                                 "CAUSE"        #{"cause"}
                                 "HUMAN"        #{"human"}
                                 "NATURE"       #{"nature"}
                                 ;; ------------------------------------------
                                 "ENERGY"       #{"energy"}
                                 "CONSERVATION" #{"conservation"}}
                                 )

(defonce Concept-Words          (word/synonym-values Concept-Triggers))     ; Concept expansion
(defonce Concept-Stems          (update-values Concept-Words #(tw/stem-all % :set)))


;;; TODO for keywords
;;; - Model Key-Words as #{general alarmed ... dismissive}
;;; - handle bigrams & trigrams

(defonce Question-Words     {:beliefs
                             #{"caused" "human" "activities" "natural"              ; Table 5
                               "changes" "environment" "happening"}})


(defonce Key-Words  {:six36 (set/union
                             #{"change" "mind" "global" "warming" "issues"          ; Table 2
                               "information" "opinion"}

                             #{"experts" "global" "warming" "really" "happening"    ; Table 3
                               "cause" "harm" "United" "States" "reduce"}

                             #{"global" "warming" "conserving" "energy"}            ; Table 4

                             (Question-Words :beliefs)                              ; Table 5

                             #{"scientists" "global" "warming" "happening"
                               "disagreement"}

                             #{"informed"}

                             #{"interested" "disgusted" "helpless" "hopeful"        ; Table 6
                               "sad" "angry" "afraid" "guilty" "depressed"}

                             #{"important" "issue" "global" "warming" "today"       ; Table 7
                               "worried"}

                             #{"global" "warming" "harm" "personally" "family"      ; Table 8a
                               "community" "people" "United" "States"
                               "modern" "industrialized" "countries" "developing"
                               "future" "generations" "plant" "animal" "species"}

                             #{"global" "warming" "start" "harm" "people" "United"  ; Table 8b
                               "States" "now" "years" "never" "world"}

                             #{"personally" "experienced" "effects" "global"        ; Table 9
                               "warming" "record" "snowstorms" "winter" "eastern"
                               "United" "States" "question" "occurring" "local"
                               "area" "snow" "rain" "different" "normal" "warmer"
                               "colder"}

                             #{"economic" "downturn" "country" "hurt" "family"      ; Table 10
                               "economy" "bad" "shape" "US" "afford" "reduce"
                               "global" "warming"}

                             #{"protecting" "environment" "economic" "growth"       ; Table 11
                               "provides" "jobs" "reduces" "conflict"
                               "environmental" "protection" "important" "problems"}

                             #{; BIGRAMS: "turn off" "air conditioning"             ; Tables 12,13,17,18
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

                             #{; BIGRAMS: "United States"                           ; Table 14
                               "energy-saving" "actions" "months" "reduce"
                               "personal" "contribution" "global" "warming"
                               "United" "States" "people" "modern" "industrialized"
                               "countries" "world"}

                             #{; BIGRAMS: "taking steps"                            ; Table 15
                               "rewarde" "companies" "steps" "reduce" "global"
                               "warming" "buy" "products" "punish" "opposing"}

                             #{; BIGRAMS:                                           ; Table 16
                               "volunteere" "donate" "money" "organization"
                               "working" "reduce" "global" "warming" "posted"
                               "comment" "online" "response" "news" "story" "blog"
                               "written" "letters" "emailed" "phoned" "government"
                               "officials" "contacted" "urged" "take" "action"
                               "volunteer"}

                             #{; BIGRAMS:                                           ; Table 19
                               "home" "family" "friends" "people" "global" "warming"
                               "spoken" "share" "views"}

                             #{; BIGRAMS: "saving energy"                           ; Table 20
                               "discuss" "global" "warming" "children" "agree"
                               "saving" "energy"}

                             #{; BIGRAMS:                                           ; Table 21
                               "people" "spoken" "global" "warming" "information"
                               "advice"}

                             #{; BIGRAMS:                                           ; Table 22
                               "view" "humans" "reduce" "global" "warming"
                               "successfully" "unclear" "change" "behavior"
                               "happening" "actions" "single" "individual"
                               "difference"}

                             #{; BIGRAMS: "United States"                           ; Table 23
                               "countries" "industrialized" "England" "Germany"
                               "Japan" "reduce" "emissions" "developing" "China"
                               "India" "Brazil" "US" "United" "States" "large-scale"
                               "effort" "large" "economic" "costs" "medium-scale"
                               "moderate" "small-scale" "small"}

                             #{; BIGRAMS:                                           ; Table 24
                               "low" "medium" "high" "priority" "president"
                               "congress" "developing" "sources" "clean" "energy"}

                             #{; BIGRAMS : "tax rebates" "solar panels"             ; Table 25
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

                             #{; BIGRAMS: "news organizations"                          ; Table 26,27
                               "news" "controversial" "emails" "climate" "scientists"
                               "England" "US" "organizations" "release" "Climategate"
                               "followed" "stories" "closely" "certain" "global" "warming"
                               "happening" "influence" "trust" "errors" "Intergovernmental"
                               "Panel" "Climate" "Change" "IPCC" "report" "certainty"}

                             #{; BIGRAMS : "mainstream news" "religious leaders"        ; Table 28
                               ; TRIGRAMS: "mainstream news media"
                               "trust" "distrust" "source" "information" "global"
                               "warming" "scientists" "mainstream" "news" "media"
                               "television" "weather" "reporters" "religious" "leaders"}

                             #{; BIGRAMS : "media sources" "world events"               ; Table 29
                               ; TRIGRAMS: "point of view"         
                               "media" "sources" "current" "news" "world" "events"
                               "television" "internet" "email" "radio" "print"
                               "newspapers" "magazines" "political" "point" "view"
                               "share"}

                             #{; BIGRAMS : "TV news" "Weather Channel" "Fox News"       ; Table 30
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

                             #{; BIGRAMS : "Tea Party"                                  ; Table 31
                               ; TRIGRAMS: "middle of the road"    
                               "Republican" "Democrat" "Independent" "party" "interested"
                               "politics" "liberal" "moderate" "conservative" "Tea" "Party"
                               "movement" "registered" "vote"}
)
                     :sassy #{"think"
                              "global" "warming"
                              "harm"
                              "future" "generations" "people"
                              "important" "issue" "personally"
                              "worried"}})

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
        in?   (in-stems? hits word sball)]

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
  with this  namespace."
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
            (log/fmt-info "~16a: ~4a" stem freq))
          stems))))


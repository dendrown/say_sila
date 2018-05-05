(defproject say "0.0.1-SNAPSHOT"
  :description  "Bridge to JVM-based utilities for sila"
  :url          "http://dendrown.net"
  :license {:name "BSD 3-clause License"
            :url "https://spdx.org/licenses/BSD-3-Clause.html"}
  :main say.core
  :plugins [[lein-localrepo "0.5.4"]]
  :dependencies [[org.clojure/clojure                   "1.9.0"]
                 [com.ericsson.otp.erlang/jinterface    "1.8.1"]
                 [clj-time                              "0.14.2"]
                 ; Weka + libs
                 [weka/weka                             "3.8.2"]
                 [edu.cmu.cs/ark-tweet-nlp              "0.3.2"]
                 [it.unimi.dsi/fastutil                 "7.0.13"]
                 [uk.ac.wlv/sentistrength               "0.1.0"]
                 [affective/affectivetweets             "1.0.1"]
                 ; Ontologies
                 [uk.org.russet/tawny-owl               "1.6.0"]
                 [com.google.guava/guava                "25.0-jre"]])  ; OWL caching dep

; NOTE for external JARs:
;  lein localrepo install /usr/lib/erlang/lib/jinterface-1.8.1/priv/OtpErlang.jar com.ericsson.otp.erlang/jinterface 1.8.1
;  lein localrepo install /usr/local/java/weka-3-8-2/weka.jar weka/weka 3.8.2
;  lein localrepo install ~/wekafiles/packages/AffectiveTweets/AffectiveTweets.jar affective/affectivetweets 1.0.1
;  lein localrepo install ~/wekafiles/packages/AffectiveTweets/lib/SentiStrength.jar uk.ac.wlv/sentistrength 0.1.0

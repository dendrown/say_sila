(defproject sila-weka "0.1.0-SNAPSHOT"
  :description "Bridge to Weka ML utilities for sila"
  :url "http://dendrown.net"
  :license {:name "BSD 3-clause License"
            :url "https://spdx.org/licenses/BSD-3-Clause.html"}
  :main sila-weka.core
  :plugins [[lein-localrepo "0.5.4"]]
  :dependencies [[org.clojure/clojure                "1.8.0" ]
                 [clj-time                           "0.14.0"]
                 [edu.cmu.cs/ark-tweet-nlp           "0.3.2" ]
                 [it.unimi.dsi/fastutil              "7.0.13"]
                 [uk.ac.wlv/sentistrength            "0.1.0" ]
                 [com.ericsson.otp.erlang/jinterface "1.7.1" ]
                 [weka/weka                          "3.8.1" ]
                 [affective/affectivetweets          "1.0.1" ]])

; NOTE for external JARs:
;  lein localrepo install /usr/lib/erlang/lib/jinterface-1.7.1/priv/OtpErlang.jar com.ericsson.otp.erlang/jinterface 1.7.1
;  lein localrepo install /usr/local/java/weka-3-8-1/weka.jar weka/weka 3.8.1
;  lein localrepo install ~/wekafiles/packages/AffectiveTweets/AffectiveTweets.jar affective/affectivetweets 1.0.0
;  lein localrepo install ~/wekafiles/packages/AffectiveTweets/lib/SentiStrength.jar uk.ac.wlv/sentistrength 0.1.0

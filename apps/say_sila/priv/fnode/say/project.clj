;;; TODO Override for insecure repositories
;;;      @see https://github.com/technomancy/leiningen/blob/master/doc/FAQ.md
;;; - DL-Learner:
;;;      davidsoergel.releases: http://dev.davidsoergel.com/nexus/content/repositories/releases
(comment require 'cemerick.pomegranate.aether)
(comment cemerick.pomegranate.aether/register-wagon-factory!
  "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))


(defproject say "0.2.0-SNAPSHOT"
  :description  "Bridge to JVM-based utilities for Say-Sila"
  :url          "http://dendrown.net"
  :license {:name "BSD 3-clause License"
            :url "https://spdx.org/licenses/BSD-3-Clause.html"}
  :main say.core
  :jvm-opts ["-Xmx16G"]
  :aot [weka.classifiers.rules.DLRules
        weka.filters.unsupervised.attribute.TweetToGenderFeatures]
  :java-source-paths ["java/src"]
  :plugins [[lein-localrepo "0.5.4"]]
  :repositories [["maven.aksw.internal"  {:url "https://maven.aksw.org/repository/internal/"}]
                 ["maven.aksw.snapshots" {:url "https://maven.aksw.org/repository/snapshots/"}]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.erlang.otp/jinterface "1.9.1"]
                 [org.clojure/core.async "1.0.567"]
                 [org.clojure/core.logic "0.8.11"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [clojusc/wordnet "1.2.0"]
                 [clj-time "0.14.2"]
                 [defun "0.3.1"]
                 [enlive "1.1.6"]
                 [incanter/incanter-core "1.9.3"]
                 [incanter/incanter-charts "1.9.3"]
                 [io.jenetics/jenetics "6.0.0"]
                 [me.raynes/fs "1.4.6"]
                 ; Weka + libs
                 [nz.ac.waikato.cms.weka/weka-stable "3.8.3"]
                 [edu.cmu.cs/ark-tweet-nlp "0.3.2"]
                 [it.unimi.dsi/fastutil "7.0.13"]
                 [uk.ac.wlv/sentistrength "0.1.0"]
                 [affective/affectivetweets "1.0.2"]
                ;[org.tartarus/snowball "1.0.0"]                                ; We get Snowball with Lucene
                 ; Lucene
                 [org.apache.lucene/lucene-core "7.7.3"]
                 [org.apache.lucene/lucene-analyzers-common "7.7.3"]
                 [org.apache.lucene/lucene-queryparser "7.7.3"]
                 ; Ontologies
                ;[org.dllearner/components-core "1.4.0"]                        ; TODO: Enable
                 [net.sourceforge.owlapi/org.semanticweb.hermit "1.4.5.456"]    ; Override Tawny's dep
                 [uk.org.russet/tawny-owl "2.0.3"]
                 [com.google.guava/guava "25.0-jre"]])                          ; OWL caching dep


;-------------------------------------------------------------------------------
; External JARs [ lein localrepo install ]
;
; * Erlang jInterface:
;   /usr/lib/erlang/lib/jinterface-1.9.1/priv/OtpErlang.jar
;   org.erlang.otp/jinterface
;   1.9.1
;   NOTE: Not up-to-date @ https://mvnrepository.com/artifact/org.erlang.otp/jinterface
;
; * Snowball Stemmers
;   ~/Downloads/snowball.jar
;   org.tartarus/snowball
;   1.0.0
;
; * Weka AffectiveTweets
;   ~/wekafiles/packages/AffectiveTweets/AffectiveTweets.jar
;   affective/affectivetweets
;   1.0.2
;
; * Weka SentiStrength
;   ~/wekafiles/packages/AffectiveTweets/lib/SentiStrength.jar
;   uk.ac.wlv/sentistrength
;   0.1.0

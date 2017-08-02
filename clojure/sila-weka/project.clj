(defproject sila-weka "0.1.0-SNAPSHOT"
  :description "Bridge to Weka ML utilities for sila"
  :url "http://dendrown.net"
  :license {:name "BSD 3-clause License"
            :url "https://spdx.org/licenses/BSD-3-Clause.html"}
  :plugins [[lein-localrepo "0.5.4"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.ericsson.otp.erlang/jinterface "1.7.1"]
                 [weka/weka "3.8.1"]])

; NOTE for external JARs:
;  lein localrepo install /usr/lib/erlang/lib/jinterface-1.7.1/priv/OtpErlang.jar com.ericsson.otp.erlang/jinterface 1.7.1
;  lein localrepo install /usr/local/java/weka-3-8-1/weka.jar weka/weka 3.8.1

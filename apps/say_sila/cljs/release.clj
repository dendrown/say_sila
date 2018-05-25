(require ['cljs.build.api :as 'api])

; NOTE: advanced optimizations are interfering with
;       the SSE connection to the server
;       @ref https://clojurescript.org/reference/compiler-options#optimizations
(api/build "src" {:main          'sila.core
                  :output-to     "out/main.js"
                  :optimizations :simple})

(System/exit 0)

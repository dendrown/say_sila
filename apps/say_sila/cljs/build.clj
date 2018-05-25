(require ['cljs.build.api :as 'api])

(api/build "src" {:main         'sila.core
                  :libs         ["lib/venn.js"]
                 ;:foreign-libs [{:file "https://d3js.org/d3.v4.min.js" :provides ["js.d3"]}]
                  :output-to    "out/main.js"})

(require 'cljs.build.api)

(cljs.build.api/build "src"
                      {:main 'jp
                       :output-to "out/jp.js"
                       :optimizations :advanced})

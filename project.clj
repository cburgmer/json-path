(defproject json-path "0.3.0"
  :description "JSON Path for Clojure data structures"
  :url "http://github.com/gga/json-path"
  :dependencies [[org.clojure/clojure "1.5.0"]]
  :profiles {:dev {:plugins [[lein-midje "3.2.1"]]
                   :dependencies [[midje "1.6.0"]]}})

(defproject demo-may "0.1.0-SNAPSHOT"
  :description "repl algo"
  :url "http://github.com/FIXME"
  :license {:name "EPL" :url "/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]]
  :main ^:skip-aot demo-may.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

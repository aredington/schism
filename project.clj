(defproject com.holychao/schism "0.1.0-SNAPSHOT"
  :description "First Class CRDTs for Clojure"
  :url "https://github.com/aredington/schism"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]
  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-doo "0.1.8"]]
  :profiles {:dev {:dependencies [[doo "0.1.8"]]}}
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to     "target/test.js"
                                   :main schism.test
                                   :output-dir    "target"
                                   :optimizations :none
                                   :source-map    true
                                   :pretty-print  true
                                   :recompile-dependents false
                                   :parallel-build true
                                   :checked-arrays :warn}}]}
  :clean-targets ^{:protect false} ["target"]
  :aliases {"test-platforms" ["do" "clean," "test," "doo" "chrome" "test" "once"]})

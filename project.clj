(defproject com.holychao/schism "0.1.0"
  :description "First Class CRDTs for Clojure"
  :url "https://github.com/aredington/schism"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.520" :scope "provided"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.11"]]
  :profiles {:dev {:dependencies [[doo "0.1.11"]
                                  [org.clojure/test.check "0.10.0-alpha4"]]}}
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
  :aliases {"test-platforms" ["do" "clean," "test," "doo" "chrome-headless" "test" "once"]})

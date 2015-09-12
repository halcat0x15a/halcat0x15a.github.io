(defproject halcat-org "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]]
  :plugins [[lein-cljsbuild "1.1.0"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :cljsbuild {:builds [{:source-paths ["src/main/cljs"]
                        :compiler {:output-to "out/main.js"}}]})

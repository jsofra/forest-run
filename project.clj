(defproject forest-run "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [impi "0.0.11-SNAPSHOT"]
                 [org.clojure/core.async "0.4.474"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]]
  :cljsbuild
  {:builds
   {:main
    {:figwheel     true
     :source-paths ["src"]
     :compiler     {:main          forest-run.ui
                    :asset-path    "js/out"
                    :output-to     "resources/public/js/main.js"
                    :output-dir    "resources/public/js/out"
                    :optimizations :none}}
    :demo
    {:source-paths ["src"]
     :compiler     {:main          forest-run.ui
                    :output-to     "demo.js"
                    :optimizations :simple}}}}
  :figwheel
  {:http-server-root "public"
   :server-port      3001
   :css-dirs         ["resources/public/css"]}
  :profiles {:dev {:dependencies [[binaryage/devtools "0.9.10"]
                                  [figwheel-sidecar "0.5.16" :exclusions [org.clojure/tools.nrepl]]
                                  [cider/piggieback "0.3.8"]]
                   :repl-options {:init (set! *print-length* 50)}}})

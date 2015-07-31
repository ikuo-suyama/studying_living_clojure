(defproject studing_living_clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [camel-snake-kebab "0.2.4"]]
  :main ^:skip-aot studing-living-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

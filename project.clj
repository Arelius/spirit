(defproject spirit "0.1.0-SNAPSHOT"
  :description "User Server."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [postgresql/postgresql "8.4-701.jdbc4"]
                 [jbcrypt "0.3"]
                 [ring/ring-core "0.2.0"]
                 [ring/ring-jetty-adapter "0.2.0"]
                 [clout "0.4.0"]
                 [org.danlarkin/clojure-json "1.2-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [ring/ring-devel "0.2.0"]])

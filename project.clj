(defproject postfix-notation "0.1.0-SNAPSHOT"
  :description "evaluate postfix/prefix notation"
  :license {:name "0BSD"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "1.1.0"]]
  :main ^:skip-aot postfix-notation.main
  :repl-options {:init-ns postfix-notation.main})

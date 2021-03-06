(defproject clojea "0.1.0-SNAPSHOT"
  :description "Evolutionary Algorithms in Clojure a bit differently"
  :url "https://github.com/rereverse/clojea"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :aot :all)

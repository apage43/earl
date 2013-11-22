(defproject im.crate.earl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :jvm-opts ["-Xmx8g"]
  :repl-options {:init-ns im.crate.earl}
;  :license {:name "Eclipse Public License"
;            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.erlang.otp/jinterface "1.5.6"]
                 [clj-http "0.7.7"]
                 [cheshire "5.2.0"]
                 [lonocloud/synthread "1.0.4"]
                 ])

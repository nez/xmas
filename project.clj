(defproject xmas "0.1.0"
  :description "A minimal Emacs in Clojure"
  :dependencies [[org.clojure/clojure "1.12.0"]]
  :main xmas.ed
  :aot [xmas.ed]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

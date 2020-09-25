(defproject open-api-petstore "1.0.0"
  :description "This is a sample server Petstore server. For this sample, you can use the api key \"special-key\" to test the authorization filters"
  :license {:name "Apache-2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [metosin/spec-tools "0.7.0"]
                 [clj-http "3.8.0"]
                 [orchestra "2017.11.12-1"]
                 [cheshire "5.8.0"]])
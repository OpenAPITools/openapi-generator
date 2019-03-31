(ns open-api-petstore.specs.category
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def category-data
  {
   (ds/opt :id) int?
   (ds/opt :name) string?
   })

(def category-spec
  (ds/spec
    {:name ::category
     :spec category-data}))

(ns open-api-petstore.specs.Category
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def Category
  {
   (ds/opt :id) int?
   (ds/opt :name) string?
   })

(def Category-spec
  (ds/spec
    {:name ::Category
     :spec Category}))

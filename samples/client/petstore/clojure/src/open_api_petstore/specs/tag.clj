(ns open-api-petstore.specs.tag
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def tag-data
  {
   (ds/opt :id) int?
   (ds/opt :name) string?
   })

(def tag-spec
  (ds/spec
    {:name ::tag
     :spec tag-data}))

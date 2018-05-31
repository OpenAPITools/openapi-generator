(ns open-api-petstore.specs.Tag
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def Tag
  {
   (ds/opt :id) int?
   (ds/opt :name) string?
   })

(def Tag-spec
  (ds/spec
    {:name ::Tag
     :spec Tag}))

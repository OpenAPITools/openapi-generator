(ns open-api-petstore.specs.inline-object-1
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def inline-object-1-data
  {
   (ds/opt :additionalMetadata) string?
   (ds/opt :file) any?
   })

(def inline-object-1-spec
  (ds/spec
    {:name ::inline-object-1
     :spec inline-object-1-data}))

(ns open-api-petstore.specs.inline-object
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def inline-object-data
  {
   (ds/opt :name) string?
   (ds/opt :status) string?
   })

(def inline-object-spec
  (ds/spec
    {:name ::inline-object
     :spec inline-object-data}))

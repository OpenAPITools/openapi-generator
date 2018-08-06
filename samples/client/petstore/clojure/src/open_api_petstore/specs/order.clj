(ns open-api-petstore.specs.order
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def order-data
  {
   (ds/opt :id) int?
   (ds/opt :petId) int?
   (ds/opt :quantity) int?
   (ds/opt :shipDate) inst?
   (ds/opt :status) string?
   (ds/opt :complete) boolean?
   })

(def order-spec
  (ds/spec
    {:name ::order
     :spec order-data}))

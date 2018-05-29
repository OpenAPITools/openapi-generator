(ns open-api-petstore.specs.Order
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def Order
  {
   (ds/opt :id) int?
   (ds/opt :petId) int?
   (ds/opt :quantity) int?
   (ds/opt :shipDate) inst?
   (ds/opt :status) string?
   (ds/opt :complete) boolean?
   })

(def Order-spec
  (ds/spec
    {:name ::Order
     :spec Order}))

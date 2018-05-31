(ns open-api-petstore.specs.Pet
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            [open-api-petstore.specs.Category :refer :all]
            [open-api-petstore.specs.Tag :refer :all]
            )
  (:import (java.io File)))


(def Pet
  {
   (ds/opt :id) int?
   (ds/opt :category) Category-spec
   (ds/req :name) string?
   (ds/req :photoUrls) (s/coll-of string?)
   (ds/opt :tags) (s/coll-of Tag-spec)
   (ds/opt :status) string?
   })

(def Pet-spec
  (ds/spec
    {:name ::Pet
     :spec Pet}))

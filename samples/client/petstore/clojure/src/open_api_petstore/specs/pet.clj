(ns open-api-petstore.specs.pet
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            [open-api-petstore.specs.category :refer :all]
            [open-api-petstore.specs.tag :refer :all]
            )
  (:import (java.io File)))


(def pet-data
  {
   (ds/opt :id) int?
   (ds/opt :category) category-spec
   (ds/req :name) string?
   (ds/req :photoUrls) (s/coll-of string?)
   (ds/opt :tags) (s/coll-of tag-spec)
   (ds/opt :status) string?
   })

(def pet-spec
  (ds/spec
    {:name ::pet
     :spec pet-data}))

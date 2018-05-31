(ns open-api-petstore.specs.User
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def User
  {
   (ds/opt :id) int?
   (ds/opt :username) string?
   (ds/opt :firstName) string?
   (ds/opt :lastName) string?
   (ds/opt :email) string?
   (ds/opt :password) string?
   (ds/opt :phone) string?
   (ds/opt :userStatus) int?
   })

(def User-spec
  (ds/spec
    {:name ::User
     :spec User}))

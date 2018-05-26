(ns open-api-petstore.specs.user
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            )
  (:import (java.io File)))


(def user
  {
   (ds/opt :id) int?
   (ds/opt :username) string?
   (ds/opt :first-name) string?
   (ds/opt :last-name) string?
   (ds/opt :email) string?
   (ds/opt :password) string?
   (ds/opt :phone) string?
   (ds/opt :user-status) int?
   })

(def user-spec
  (ds/spec
    {:name ::user
     :spec user}))

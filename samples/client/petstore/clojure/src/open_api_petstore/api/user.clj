(ns open-api-petstore.api.user
  (:require [open-api-petstore.core :refer [call-api check-required-params with-collection-format *api-context*]]
            [clojure.spec.alpha :as s]
            [spec-tools.core :as st]
            [orchestra.core :refer [defn-spec]]
            [open-api-petstore.specs.tag :refer :all]
            [open-api-petstore.specs.category :refer :all]
            [open-api-petstore.specs.user :refer :all]
            [open-api-petstore.specs.pet :refer :all]
            [open-api-petstore.specs.order :refer :all]
            )
  (:import (java.io File)))


(defn-spec create-user-with-http-info any?
  "Create user
  This can only be done by the logged in user."
  ([] (create-user-with-http-info nil))
  ([{:keys [user]} (s/map-of keyword? any?)]
   (call-api "/user" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    user
              :content-types []
              :accepts       []
              :auth-names    []})))

(defn-spec create-user any?
  "Create user
  This can only be done by the logged in user."
  ([] (create-user nil))
  ([optional-params any?]
   (let [res (:data (create-user-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec create-users-with-array-input-with-http-info any?
  "Creates list of users with given input array"
  ([] (create-users-with-array-input-with-http-info nil))
  ([{:keys [user]} (s/map-of keyword? any?)]
   (call-api "/user/createWithArray" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    user
              :content-types []
              :accepts       []
              :auth-names    []})))

(defn-spec create-users-with-array-input any?
  "Creates list of users with given input array"
  ([] (create-users-with-array-input nil))
  ([optional-params any?]
   (let [res (:data (create-users-with-array-input-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec create-users-with-list-input-with-http-info any?
  "Creates list of users with given input array"
  ([] (create-users-with-list-input-with-http-info nil))
  ([{:keys [user]} (s/map-of keyword? any?)]
   (call-api "/user/createWithList" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    user
              :content-types []
              :accepts       []
              :auth-names    []})))

(defn-spec create-users-with-list-input any?
  "Creates list of users with given input array"
  ([] (create-users-with-list-input nil))
  ([optional-params any?]
   (let [res (:data (create-users-with-list-input-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec delete-user-with-http-info any?
  "Delete user
  This can only be done by the logged in user."
  [username string?]
  (check-required-params username)
  (call-api "/user/{username}" :delete
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       []
             :auth-names    []}))

(defn-spec delete-user any?
  "Delete user
  This can only be done by the logged in user."
  [username string?]
  (let [res (:data (delete-user-with-http-info username))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))


(defn-spec get-user-by-name-with-http-info any?
  "Get user by user name"
  [username string?]
  (check-required-params username)
  (call-api "/user/{username}" :get
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    []}))

(defn-spec get-user-by-name user-spec
  "Get user by user name"
  [username string?]
  (let [res (:data (get-user-by-name-with-http-info username))]
    (if (:decode-models *api-context*)
       (st/decode user-spec res st/string-transformer)
       res)))


(defn-spec login-user-with-http-info any?
  "Logs user into the system"
  ([] (login-user-with-http-info nil))
  ([{:keys [username password]} (s/map-of keyword? any?)]
   (call-api "/user/login" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"username" username "password" password }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn-spec login-user string?
  "Logs user into the system"
  ([] (login-user nil))
  ([optional-params any?]
   (let [res (:data (login-user-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode string? res st/string-transformer)
        res))))


(defn-spec logout-user-with-http-info any?
  "Logs out current logged in user session"
  []
  (call-api "/user/logout" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       []
             :auth-names    []}))

(defn-spec logout-user any?
  "Logs out current logged in user session"
  []
  (let [res (:data (logout-user-with-http-info))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))


(defn-spec update-user-with-http-info any?
  "Updated user
  This can only be done by the logged in user."
  ([username string?, ] (update-user-with-http-info username nil))
  ([username string?, {:keys [user]} (s/map-of keyword? any?)]
   (check-required-params username)
   (call-api "/user/{username}" :put
             {:path-params   {"username" username }
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    user
              :content-types []
              :accepts       []
              :auth-names    []})))

(defn-spec update-user any?
  "Updated user
  This can only be done by the logged in user."
  ([username string?, ] (update-user username nil))
  ([username string?, optional-params any?]
   (let [res (:data (update-user-with-http-info username optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))



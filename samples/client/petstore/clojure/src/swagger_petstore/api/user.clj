(ns swagger-petstore.api.user
  (:require [swagger-petstore.core :refer [call-api check-required-params]])
  (:import (java.io File)))

(defn create-user
  "Create user
  This can only be done by the logged in user."
  ([] (create-user nil))
  ([{:keys [body ]}]
   (call-api "/user" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn create-users-with-array-input
  "Creates list of users with given input array
  "
  ([] (create-users-with-array-input nil))
  ([{:keys [body ]}]
   (call-api "/user/createWithArray" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn create-users-with-list-input
  "Creates list of users with given input array
  "
  ([] (create-users-with-list-input nil))
  ([{:keys [body ]}]
   (call-api "/user/createWithList" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn login-user
  "Logs user into the system
  "
  ([] (login-user nil))
  ([{:keys [username password ]}]
   (call-api "/user/login" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"username" username "password" password }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn logout-user
  "Logs out current logged in user session
  "
  []
  (call-api "/user/logout" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

(defn get-user-by-name
  "Get user by user name
  "
  [username ]
  (call-api "/user/{username}" :get
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

(defn update-user
  "Updated user
  This can only be done by the logged in user."
  ([username ] (update-user username nil))
  ([username {:keys [body ]}]
   (call-api "/user/{username}" :put
             {:path-params   {"username" username }
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn delete-user
  "Delete user
  This can only be done by the logged in user."
  [username ]
  (call-api "/user/{username}" :delete
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

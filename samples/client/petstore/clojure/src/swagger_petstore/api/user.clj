(ns swagger-petstore.api.user
  (:require [swagger-petstore.core :refer [call-api check-required-params with-collection-format]])
  (:import (java.io File)))

(defn create-user-with-http-info
  "Create user
  This can only be done by the logged in user."
  ([] (create-user-with-http-info nil))
  ([{:keys [body ]}]
   (call-api "/user" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn create-user
  "Create user
  This can only be done by the logged in user."
  ([] (create-user nil))
  ([optional-params]
   (:data (create-user-with-http-info optional-params))))

(defn create-users-with-array-input-with-http-info
  "Creates list of users with given input array
  "
  ([] (create-users-with-array-input-with-http-info nil))
  ([{:keys [body ]}]
   (call-api "/user/createWithArray" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn create-users-with-array-input
  "Creates list of users with given input array
  "
  ([] (create-users-with-array-input nil))
  ([optional-params]
   (:data (create-users-with-array-input-with-http-info optional-params))))

(defn create-users-with-list-input-with-http-info
  "Creates list of users with given input array
  "
  ([] (create-users-with-list-input-with-http-info nil))
  ([{:keys [body ]}]
   (call-api "/user/createWithList" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn create-users-with-list-input
  "Creates list of users with given input array
  "
  ([] (create-users-with-list-input nil))
  ([optional-params]
   (:data (create-users-with-list-input-with-http-info optional-params))))

(defn delete-user-with-http-info
  "Delete user
  This can only be done by the logged in user."
  [username ]
  (check-required-params username)
  (call-api "/user/{username}" :delete
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    []}))

(defn delete-user
  "Delete user
  This can only be done by the logged in user."
  [username ]
  (:data (delete-user-with-http-info username)))

(defn get-user-by-name-with-http-info
  "Get user by user name
  "
  [username ]
  (check-required-params username)
  (call-api "/user/{username}" :get
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    []}))

(defn get-user-by-name
  "Get user by user name
  "
  [username ]
  (:data (get-user-by-name-with-http-info username)))

(defn login-user-with-http-info
  "Logs user into the system
  "
  ([] (login-user-with-http-info nil))
  ([{:keys [username password ]}]
   (call-api "/user/login" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"username" username "password" password }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn login-user
  "Logs user into the system
  "
  ([] (login-user nil))
  ([optional-params]
   (:data (login-user-with-http-info optional-params))))

(defn logout-user-with-http-info
  "Logs out current logged in user session
  "
  []
  (call-api "/user/logout" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    []}))

(defn logout-user
  "Logs out current logged in user session
  "
  []
  (:data (logout-user-with-http-info)))

(defn update-user-with-http-info
  "Updated user
  This can only be done by the logged in user."
  ([username ] (update-user-with-http-info username nil))
  ([username {:keys [body ]}]
   (check-required-params username)
   (call-api "/user/{username}" :put
             {:path-params   {"username" username }
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn update-user
  "Updated user
  This can only be done by the logged in user."
  ([username ] (update-user username nil))
  ([username optional-params]
   (:data (update-user-with-http-info username optional-params))))


(ns swagger-petstore.api.pet
  (:require [swagger-petstore.core :refer [call-api check-required-params with-collection-format]])
  (:import (java.io File)))

(defn add-pet-with-http-info
  "Add a new pet to the store
  "
  ([] (add-pet-with-http-info nil))
  ([{:keys [body ]}]
   (call-api "/pet" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types ["application/json" "application/xml"]
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn add-pet
  "Add a new pet to the store
  "
  ([] (add-pet nil))
  ([optional-params]
   (:data (add-pet-with-http-info optional-params))))

(defn delete-pet-with-http-info
  "Deletes a pet
  "
  ([pet-id ] (delete-pet-with-http-info pet-id nil))
  ([pet-id {:keys [api-key ]}]
   (call-api "/pet/{petId}" :delete
             {:path-params   {"petId" pet-id }
              :header-params {"api_key" api-key }
              :query-params  {}
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn delete-pet
  "Deletes a pet
  "
  ([pet-id ] (delete-pet pet-id nil))
  ([pet-id optional-params]
   (:data (delete-pet-with-http-info pet-id optional-params))))

(defn find-pets-by-status-with-http-info
  "Finds Pets by status
  Multiple status values can be provided with comma separated strings"
  ([] (find-pets-by-status-with-http-info nil))
  ([{:keys [status ]}]
   (call-api "/pet/findByStatus" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"status" (with-collection-format status :multi) }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn find-pets-by-status
  "Finds Pets by status
  Multiple status values can be provided with comma separated strings"
  ([] (find-pets-by-status nil))
  ([optional-params]
   (:data (find-pets-by-status-with-http-info optional-params))))

(defn find-pets-by-tags-with-http-info
  "Finds Pets by tags
  Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."
  ([] (find-pets-by-tags-with-http-info nil))
  ([{:keys [tags ]}]
   (call-api "/pet/findByTags" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"tags" (with-collection-format tags :multi) }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn find-pets-by-tags
  "Finds Pets by tags
  Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."
  ([] (find-pets-by-tags nil))
  ([optional-params]
   (:data (find-pets-by-tags-with-http-info optional-params))))

(defn get-pet-by-id-with-http-info
  "Find pet by ID
  Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions"
  [pet-id ]
  (call-api "/pet/{petId}" :get
            {:path-params   {"petId" pet-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    ["api_key" "petstore_auth"]}))

(defn get-pet-by-id
  "Find pet by ID
  Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions"
  [pet-id ]
  (:data (get-pet-by-id-with-http-info pet-id)))

(defn update-pet-with-http-info
  "Update an existing pet
  "
  ([] (update-pet-with-http-info nil))
  ([{:keys [body ]}]
   (call-api "/pet" :put
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types ["application/json" "application/xml"]
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn update-pet
  "Update an existing pet
  "
  ([] (update-pet nil))
  ([optional-params]
   (:data (update-pet-with-http-info optional-params))))

(defn update-pet-with-form-with-http-info
  "Updates a pet in the store with form data
  "
  ([pet-id ] (update-pet-with-form-with-http-info pet-id nil))
  ([pet-id {:keys [name status ]}]
   (call-api "/pet/{petId}" :post
             {:path-params   {"petId" pet-id }
              :header-params {}
              :query-params  {}
              :form-params   {"name" name "status" status }
              :content-types ["application/x-www-form-urlencoded"]
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn update-pet-with-form
  "Updates a pet in the store with form data
  "
  ([pet-id ] (update-pet-with-form pet-id nil))
  ([pet-id optional-params]
   (:data (update-pet-with-form-with-http-info pet-id optional-params))))

(defn upload-file-with-http-info
  "uploads an image
  "
  ([pet-id ] (upload-file-with-http-info pet-id nil))
  ([pet-id {:keys [additional-metadata ^File file ]}]
   (call-api "/pet/{petId}/uploadImage" :post
             {:path-params   {"petId" pet-id }
              :header-params {}
              :query-params  {}
              :form-params   {"additionalMetadata" additional-metadata "file" file }
              :content-types ["multipart/form-data"]
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn upload-file
  "uploads an image
  "
  ([pet-id ] (upload-file pet-id nil))
  ([pet-id optional-params]
   (:data (upload-file-with-http-info pet-id optional-params))))


(ns swagger-petstore.api.pet
  (:require [swagger-petstore.core :refer [call-api check-required-params]])
  (:import (java.io File)))

(defn update-pet
  "Update an existing pet
  "
  ([] (update-pet nil))
  ([{:keys [body ]}]
   (call-api "/pet" :put
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types ["application/json" "application/xml"]
              :accepts       ["application/json" "application/xml"]})))

(defn add-pet
  "Add a new pet to the store
  "
  ([] (add-pet nil))
  ([{:keys [body ]}]
   (call-api "/pet" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types ["application/json" "application/xml"]
              :accepts       ["application/json" "application/xml"]})))

(defn find-pets-by-status
  "Finds Pets by status
  Multiple status values can be provided with comma seperated strings"
  ([] (find-pets-by-status nil))
  ([{:keys [status ]}]
   (call-api "/pet/findByStatus" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"status" status }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn find-pets-by-tags
  "Finds Pets by tags
  Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing."
  ([] (find-pets-by-tags nil))
  ([{:keys [tags ]}]
   (call-api "/pet/findByTags" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"tags" tags }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn get-pet-by-id
  "Find pet by ID
  Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions"
  [pet-id ]
  (call-api "/pet/{petId}" :get
            {:path-params   {"petId" pet-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

(defn update-pet-with-form
  "Updates a pet in the store with form data
  "
  ([pet-id ] (update-pet-with-form pet-id nil))
  ([pet-id {:keys [name status ]}]
   (call-api "/pet/{petId}" :post
             {:path-params   {"petId" pet-id }
              :header-params {}
              :query-params  {}
              :form-params   {"name" name "status" status }
              :content-types ["application/x-www-form-urlencoded"]
              :accepts       ["application/json" "application/xml"]})))

(defn delete-pet
  "Deletes a pet
  "
  ([pet-id ] (delete-pet pet-id nil))
  ([pet-id {:keys [api-key ]}]
   (call-api "/pet/{petId}" :delete
             {:path-params   {"petId" pet-id }
              :header-params {"api_key" api-key }
              :query-params  {}
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn upload-file
  "uploads an image
  "
  ([pet-id ] (upload-file pet-id nil))
  ([pet-id {:keys [additional-metadata ^File file ]}]
   (call-api "/pet/{petId}/uploadImage" :post
             {:path-params   {"petId" pet-id }
              :header-params {}
              :query-params  {}
              :form-params   {"additionalMetadata" additional-metadata "file" file }
              :content-types ["multipart/form-data"]
              :accepts       ["application/json" "application/xml"]})))

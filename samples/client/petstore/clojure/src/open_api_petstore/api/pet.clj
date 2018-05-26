(ns open-api-petstore.api.pet
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


(defn-spec add-pet-with-http-info any?
  "Add a new pet to the store"
  ([] (add-pet-with-http-info nil))
  ([{:keys [pet]}]
   (call-api "/pet" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    pet
              :content-types ["application/json" "application/xml"]
              :accepts       []
              :auth-names    ["petstore_auth"]})))

(defn-spec add-pet any?
  "Add a new pet to the store"
  ([] (add-pet nil))
  ([optional-params]
   (let [res (:data (add-pet-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec delete-pet-with-http-info any?
  "Deletes a pet"
  ([pet-id int?, ] (delete-pet-with-http-info pet-id nil))
  ([pet-id int?, {:keys [api-key]}]
   (check-required-params pet-id)
   (call-api "/pet/{petId}" :delete
             {:path-params   {"petId" pet-id }
              :header-params {"api_key" api-key }
              :query-params  {}
              :form-params   {}
              :content-types []
              :accepts       []
              :auth-names    ["petstore_auth"]})))

(defn-spec delete-pet any?
  "Deletes a pet"
  ([pet-id int?, ] (delete-pet pet-id nil))
  ([pet-id int?, optional-params]
   (let [res (:data (delete-pet-with-http-info pet-id optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec find-pets-by-status-with-http-info any?
  "Finds Pets by status
  Multiple status values can be provided with comma separated strings"
  ([] (find-pets-by-status-with-http-info nil))
  ([{:keys [status]}]
   (call-api "/pet/findByStatus" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"status" (with-collection-format status :csv) }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn-spec find-pets-by-status (s/coll-of pet-spec)
  "Finds Pets by status
  Multiple status values can be provided with comma separated strings"
  ([] (find-pets-by-status nil))
  ([optional-params]
   (let [res (:data (find-pets-by-status-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode (s/coll-of pet-spec) res st/string-transformer)
        res))))


(defn-spec find-pets-by-tags-with-http-info any?
  "Finds Pets by tags
  Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."
  ([] (find-pets-by-tags-with-http-info nil))
  ([{:keys [tags]}]
   (call-api "/pet/findByTags" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"tags" (with-collection-format tags :csv) }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["petstore_auth"]})))

(defn-spec find-pets-by-tags (s/coll-of pet-spec)
  "Finds Pets by tags
  Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."
  ([] (find-pets-by-tags nil))
  ([optional-params]
   (let [res (:data (find-pets-by-tags-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode (s/coll-of pet-spec) res st/string-transformer)
        res))))


(defn-spec get-pet-by-id-with-http-info any?
  "Find pet by ID
  Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions"
  [pet-id int?]
  (check-required-params pet-id)
  (call-api "/pet/{petId}" :get
            {:path-params   {"petId" pet-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    ["api_key" "petstore_auth"]}))

(defn-spec get-pet-by-id pet-spec
  "Find pet by ID
  Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions"
  [pet-id int?]
  (let [res (:data (get-pet-by-id-with-http-info pet-id))]
    (if (:decode-models *api-context*)
       (st/decode pet-spec res st/string-transformer)
       res)))


(defn-spec update-pet-with-http-info any?
  "Update an existing pet"
  ([] (update-pet-with-http-info nil))
  ([{:keys [pet]}]
   (call-api "/pet" :put
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    pet
              :content-types ["application/json" "application/xml"]
              :accepts       []
              :auth-names    ["petstore_auth"]})))

(defn-spec update-pet any?
  "Update an existing pet"
  ([] (update-pet nil))
  ([optional-params]
   (let [res (:data (update-pet-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec update-pet-with-form-with-http-info any?
  "Updates a pet in the store with form data"
  ([pet-id string?, ] (update-pet-with-form-with-http-info pet-id nil))
  ([pet-id string?, {:keys [name status]}]
   (check-required-params pet-id)
   (call-api "/pet/{petId}" :post
             {:path-params   {"petId" pet-id }
              :header-params {}
              :query-params  {}
              :form-params   {"name" name "status" status }
              :content-types ["application/x-www-form-urlencoded"]
              :accepts       []
              :auth-names    ["petstore_auth"]})))

(defn-spec update-pet-with-form any?
  "Updates a pet in the store with form data"
  ([pet-id string?, ] (update-pet-with-form pet-id nil))
  ([pet-id string?, optional-params]
   (let [res (:data (update-pet-with-form-with-http-info pet-id optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))


(defn-spec upload-file-with-http-info any?
  "uploads an image"
  ([pet-id int?, ] (upload-file-with-http-info pet-id nil))
  ([pet-id int?, {:keys [additional-metadata ^File file]}]
   (check-required-params pet-id)
   (call-api "/pet/{petId}/uploadImage" :post
             {:path-params   {"petId" pet-id }
              :header-params {}
              :query-params  {}
              :form-params   {"additionalMetadata" additional-metadata "file" file }
              :content-types ["multipart/form-data"]
              :accepts       []
              :auth-names    ["petstore_auth"]})))

(defn-spec upload-file any?
  "uploads an image"
  ([pet-id int?, ] (upload-file pet-id nil))
  ([pet-id int?, optional-params]
   (let [res (:data (upload-file-with-http-info pet-id optional-params))]
     (if (:decode-models *api-context*)
        (st/decode any? res st/string-transformer)
        res))))



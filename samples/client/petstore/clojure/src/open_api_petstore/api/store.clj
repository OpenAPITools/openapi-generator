(ns open-api-petstore.api.store
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


(defn-spec delete-order-with-http-info any?
  "Delete purchase order by ID
  For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"
  [orderId string?]
  (check-required-params orderId)
  (call-api "/store/order/{orderId}" :delete
            {:path-params   {"orderId" orderId }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       []
             :auth-names    []}))

(defn-spec delete-order any?
  "Delete purchase order by ID
  For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"
  [orderId string?]
  (let [res (:data (delete-order-with-http-info orderId))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))


(defn-spec get-inventory-with-http-info any?
  "Returns pet inventories by status
  Returns a map of status codes to quantities"
  []
  (call-api "/store/inventory" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    ["api_key"]}))

(defn-spec get-inventory (s/map-of string? int?)
  "Returns pet inventories by status
  Returns a map of status codes to quantities"
  []
  (let [res (:data (get-inventory-with-http-info))]
    (if (:decode-models *api-context*)
       (st/decode (s/map-of string? int?) res st/string-transformer)
       res)))


(defn-spec get-order-by-id-with-http-info any?
  "Find purchase order by ID
  For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions"
  [orderId string?]
  (check-required-params orderId)
  (call-api "/store/order/{orderId}" :get
            {:path-params   {"orderId" orderId }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    []}))

(defn-spec get-order-by-id order-spec
  "Find purchase order by ID
  For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions"
  [orderId string?]
  (let [res (:data (get-order-by-id-with-http-info orderId))]
    (if (:decode-models *api-context*)
       (st/decode order-spec res st/string-transformer)
       res)))


(defn-spec place-order-with-http-info any?
  "Place an order for a pet"
  ([] (place-order-with-http-info nil))
  ([{:keys [order]} (s/map-of keyword? any?)]
   (call-api "/store/order" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    order
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    []})))

(defn-spec place-order order-spec
  "Place an order for a pet"
  ([] (place-order nil))
  ([optional-params any?]
   (let [res (:data (place-order-with-http-info optional-params))]
     (if (:decode-models *api-context*)
        (st/decode order-spec res st/string-transformer)
        res))))



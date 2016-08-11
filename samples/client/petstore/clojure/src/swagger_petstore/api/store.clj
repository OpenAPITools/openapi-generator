(ns swagger-petstore.api.store
  (:require [swagger-petstore.core :refer [call-api check-required-params with-collection-format]])
  (:import (java.io File)))

(defn delete-order-with-http-info
  "Delete purchase order by ID
  For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"
  [order-id ]
  (call-api "/store/order/{orderId}" :delete
            {:path-params   {"orderId" order-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    []}))

(defn delete-order
  "Delete purchase order by ID
  For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"
  [order-id ]
  (:data (delete-order-with-http-info order-id)))

(defn find-orders-by-status-with-http-info
  "Finds orders by status
  A single status value can be provided as a string"
  ([] (find-orders-by-status-with-http-info nil))
  ([{:keys [status ]}]
   (call-api "/store/findByStatus" :get
             {:path-params   {}
              :header-params {}
              :query-params  {"status" status }
              :form-params   {}
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["test_api_client_id" "test_api_client_secret"]})))

(defn find-orders-by-status
  "Finds orders by status
  A single status value can be provided as a string"
  ([] (find-orders-by-status nil))
  ([optional-params]
   (:data (find-orders-by-status-with-http-info optional-params))))

(defn get-inventory-with-http-info
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

(defn get-inventory
  "Returns pet inventories by status
  Returns a map of status codes to quantities"
  []
  (:data (get-inventory-with-http-info)))

(defn get-inventory-in-object-with-http-info
  "Fake endpoint to test arbitrary object return by 'Get inventory'
  Returns an arbitrary object which is actually a map of status codes to quantities"
  []
  (call-api "/store/inventory?response=arbitrary_object" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    ["api_key"]}))

(defn get-inventory-in-object
  "Fake endpoint to test arbitrary object return by 'Get inventory'
  Returns an arbitrary object which is actually a map of status codes to quantities"
  []
  (:data (get-inventory-in-object-with-http-info)))

(defn get-order-by-id-with-http-info
  "Find purchase order by ID
  For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions"
  [order-id ]
  (call-api "/store/order/{orderId}" :get
            {:path-params   {"orderId" order-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]
             :auth-names    ["test_api_key_header" "test_api_key_query"]}))

(defn get-order-by-id
  "Find purchase order by ID
  For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions"
  [order-id ]
  (:data (get-order-by-id-with-http-info order-id)))

(defn place-order-with-http-info
  "Place an order for a pet
  "
  ([] (place-order-with-http-info nil))
  ([{:keys [body ]}]
   (call-api "/store/order" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]
              :auth-names    ["test_api_client_id" "test_api_client_secret"]})))

(defn place-order
  "Place an order for a pet
  "
  ([] (place-order nil))
  ([optional-params]
   (:data (place-order-with-http-info optional-params))))

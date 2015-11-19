(ns swagger-petstore.api.store
  (:require [swagger-petstore.core :refer [call-api check-required-params]])
  (:import (java.io File)))

(defn get-inventory
  "Returns pet inventories by status
  Returns a map of status codes to quantities"
  []
  (call-api "/store/inventory" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

(defn place-order
  "Place an order for a pet
  "
  ([] (place-order nil))
  ([{:keys [body ]}]
   (call-api "/store/order" :post
             {:path-params   {}
              :header-params {}
              :query-params  {}
              :form-params   {}
              :body-param    body
              :content-types []
              :accepts       ["application/json" "application/xml"]})))

(defn get-order-by-id
  "Find purchase order by ID
  For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions"
  [order-id ]
  (call-api "/store/order/{orderId}" :get
            {:path-params   {"orderId" order-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

(defn delete-order
  "Delete purchase order by ID
  For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors"
  [order-id ]
  (call-api "/store/order/{orderId}" :delete
            {:path-params   {"orderId" order-id }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json" "application/xml"]}))

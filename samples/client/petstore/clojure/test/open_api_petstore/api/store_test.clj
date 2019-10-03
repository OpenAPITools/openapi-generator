(ns open-api-petstore.api.store-test
  (:require [clojure.test :refer :all]
            [open-api-petstore.core :refer [with-api-context]]
            [open-api-petstore.api.store :refer :all])
  (:import (java.util Date)))

(defn credentials-fixture [f]
  (with-api-context {:auths {"api_key" "special-key"}}
    (f)))

(use-fixtures :once credentials-fixture)

(defn- make-random-order []
  {:id       (+ 90000 (rand-int 10000))
   :petId    200
   :quantity 13
   :shipDate (Date.)
   :status   "placed"
   :complete true})

(deftest test-get-inventory
  (let [inventory (get-inventory)]
    (is (pos? (count inventory)))))

(deftest test-place-and-delete-order
  (let [order (make-random-order)
        order-id (:id order)
        _ (place-order {:order order})
        fetched (get-order-by-id order-id)]
    (doseq [attr [:id :petId :quantity]]
      (is (= (attr order) (attr fetched))))
    (delete-order order-id)
    (comment "it seems that delete-order does not really delete the order"
             (is (thrown? RuntimeException (get-order-by-id order-id))))))

(deftest test-order-spec-conforming
  (with-api-context {:decode-models true}
    (let [order (make-random-order)
          order-id (:id order)
          _ (place-order {:order order})
          fetched (get-order-by-id order-id)]
      (is (= order fetched)))))
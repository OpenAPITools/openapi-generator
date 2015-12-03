(ns swagger-petstore.api.user-test
  (:require [clojure.test :refer :all]
            [swagger-petstore.core :refer [with-api-context]]
            [swagger-petstore.api.user :refer :all]))

(defn credentials-fixture [f]
  (with-api-context {:auths {"api_key" "special-key"}}
    (f)))

(use-fixtures :once credentials-fixture)

(defn- make-random-user
  ([] (make-random-user nil))
  ([{:keys [id] :as attrs :or {id (System/currentTimeMillis)}}]
   (merge {:id         id
           :username   (str "user-" id)
           :password   "my-password"
           :userStatus 0}
          attrs)))

(deftest test-create-and-delete-user
  (let [user (make-random-user)
        username (:username user)
        _ (create-user {:body user})
        fetched (get-user-by-name username)]
    (doseq [attr [:id :username :password :userStatus]]
      (is (= (attr user) (attr fetched))))
    (delete-user username)
    (is (thrown? RuntimeException (get-user-by-name username)))))

(deftest test-create-users-with-array-input
  (let [id1 (System/currentTimeMillis)
        id2 (inc id1)
        user1 (make-random-user {:id id1})
        user2 (make-random-user {:id id2})]
    (create-users-with-array-input {:body [user1 user2]})
    (let [fetched (get-user-by-name (:username user1))]
      (is (= id1 (:id fetched))))
    (let [fetched (get-user-by-name (:username user2))]
      (is (= id2 (:id fetched))))
    (delete-user (:username user1))
    (delete-user (:username user2))))

(deftest test-create-users-with-list-input
  (let [id1 (System/currentTimeMillis)
        id2 (inc id1)
        user1 (make-random-user {:id id1})
        user2 (make-random-user {:id id2})]
    (create-users-with-list-input {:body [user1 user2]})
    (let [fetched (get-user-by-name (:username user1))]
      (is (= id1 (:id fetched))))
    (let [fetched (get-user-by-name (:username user2))]
      (is (= id2 (:id fetched))))
    (delete-user (:username user1))
    (delete-user (:username user2))))

(deftest test-login-and-lougout-user
  (let [{:keys [username password] :as user} (make-random-user)
        _ (create-user {:body user})
        result (login-user {:username username :password password})]
    (is (re-matches #"logged in user session:.+" result))
    ;; no error with logout-user
    (logout-user)
    (delete-user username)))

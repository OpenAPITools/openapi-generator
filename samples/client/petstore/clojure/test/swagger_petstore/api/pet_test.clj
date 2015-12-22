(ns swagger-petstore.api.pet-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [swagger-petstore.core :refer [with-api-context]]
            [swagger-petstore.api.pet :refer :all]))

(defn credentials-fixture [f]
  (with-api-context {:auths {"api_key" "special-key"}}
    (f)))

(use-fixtures :once credentials-fixture)

(defn- make-random-pet
  ([] (make-random-pet nil))
  ([{:keys [id] :as attrs :or {id (System/currentTimeMillis)}}]
   (merge {:id        id
           :name      (str "pet-" id)
           :status    "available"
           :photoUrls ["http://foo.bar.com/1" "http://foo.bar.com/2"]
           :category  {:name "really-happy"}}
          attrs)))

(deftest test-create-and-get-pet
  (let [{:keys [id] :as pet} (make-random-pet)
        _ (add-pet {:body pet})
        fetched (get-pet-by-id id)]
    (is (identity fetched))
    (is (= id (:id fetched)))
    (is (identity (:category fetched)))
    (is (= (get-in pet [:category :name]) (get-in fetched [:category :name])))
    (delete-pet id)))

(deftest test-create-and-get-pet-with-http-info
  (let [{:keys [id] :as pet} (make-random-pet)
        _ (add-pet-with-http-info {:body pet})
        {:keys [status headers data]} (get-pet-by-id-with-http-info id)]
    (is (= 200 status))
    (is (= "application/json" (:content-type headers)))
    (is (identity data))
    (is (= id (:id data)))
    (is (identity (:category data)))
    (is (= (get-in pet [:category :name]) (get-in data [:category :name])))
    (delete-pet id)))

(deftest test-find-pets-by-status
  (let [status "pending"
        {:keys [id] :as pet} (make-random-pet {:status status})
        _ (add-pet {:body pet})
        pets (find-pets-by-status {:status [status]})]
    (is (seq pets))
    (is (some #{id} (map :id pets)))
    (delete-pet id)))

(deftest test-find-pets-by-tags
  (let [tag-name (str "tag-" (rand-int 1000))
        tag {:name tag-name}
        {:keys [id] :as pet} (make-random-pet {:tags [tag]})
        _ (add-pet {:body pet})
        pets (find-pets-by-tags {:tags [tag-name]})]
    (is (seq pets))
    (is (some #{id} (map :id pets)))
    (delete-pet id)))

(deftest test-update-pet-with-form
  (let [{pet-id :id :as pet} (make-random-pet {:name "new name" :status "available"})
        _ (add-pet {:body pet})
        {:keys [id name status]} (get-pet-by-id pet-id)]
    (is (= pet-id id))
    (is (= "new name" name))
    (is (= "available" status))
    ;; update "name" only
    (update-pet-with-form pet-id {:name "updated name 1"})
    (let [{:keys [id name status]} (get-pet-by-id pet-id)]
      (is (= pet-id id))
      (is (= "updated name 1" name))
      (is (= "available" status)))
    ;; update "status" only
    (update-pet-with-form pet-id {:status "pending"})
    (let [{:keys [id name status]} (get-pet-by-id pet-id)]
      (is (= pet-id id))
      (is (= "updated name 1" name))
      (is (= "pending" status)))
    ;; update both "name" and "status"
    (update-pet-with-form pet-id {:name "updated name 2" :status "sold"})
    (let [{:keys [id name status]} (get-pet-by-id pet-id)]
      (is (= pet-id id))
      (is (= "updated name 2" name))
      (is (= "sold" status)))
    (delete-pet pet-id)))

(deftest test-delete-pet
  (let [{:keys [id] :as pet} (make-random-pet)
        _ (add-pet {:body pet})
        fetched (get-pet-by-id id)]
    (is (= id (:id fetched)))
    (delete-pet id)
    (is (thrown? RuntimeException (get-pet-by-id id)))))

(deftest test-upload-file
  (let [{:keys [id] :as pet} (make-random-pet)
        _ (add-pet {:body pet})
        file (io/file (io/resource "hello.txt"))]
    ;; no errors with upload-file
    (upload-file id {:file file :additional-metadata "uploading file with clojure client"})))

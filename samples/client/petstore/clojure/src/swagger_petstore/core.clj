(ns swagger-petstore.core
  (:require [cheshire.core :refer [generate-string parse-string]]
            [clojure.string :as str]
            [clj-http.client :as client])
  (:import (com.fasterxml.jackson.core JsonParseException)
           (java.io File)
           (java.util Date TimeZone)
           (java.text SimpleDateFormat)))

(def default-api-context
  "Default API context."
  {:base-url        "http://petstore.swagger.io/v2"
   :date-format     "yyyy-MM-dd"
   :datetime-format "yyyy-MM-dd'T'HH:mm:ss.SSSXXX"
   :debug           false})

(def ^:dynamic *api-context*
  "Dynamic API context to be applied in API calls."
  default-api-context)

(defmacro with-api-context
  "A helper macro to wrap *api-context* with default values."
  [context & body]
  `(binding [*api-context* (merge *api-context* ~context)]
     ~@body))

(defmacro check-required-params
  "Throw exception if the given parameter value is nil."
  [& params]
  (->> params
       (map (fn [p]
              `(if (nil? ~p)
                 (throw (IllegalArgumentException. ~(str "The parameter \"" p "\" is required"))))))
       (list* 'do)))

(defn- make-date-format
  ([format-str] (make-date-format format-str nil))
  ([format-str time-zone]
   (let [date-format (SimpleDateFormat. format-str)]
     (when time-zone
       (.setTimeZone date-format (TimeZone/getTimeZone time-zone)))
     date-format)))

(defn format-date
  "Format the given Date object with the :date-format defined in *api-options*.
  NOTE: The UTC time zone is used."
  [^Date date]
  (let [{:keys [date-format]} *api-context*]
    (-> (make-date-format date-format "UTC")
        (.format date))))

(defn parse-date
  "Parse the given string to a Date object with the :date-format defined in *api-options*.
  NOTE: The UTC time zone is used."
  [^String s]
  (let [{:keys [date-format]} *api-context*]
    (-> (make-date-format date-format "UTC")
        (.parse s))))

(defn format-datetime
  "Format the given Date object with the :datetime-format defined in *api-options*.
  NOTE: The system's default time zone is used when not provided."
  ([^Date date] (format-datetime date nil))
  ([^Date date ^String time-zone]
   (let [{:keys [datetime-format]} *api-context*]
     (-> (make-date-format datetime-format time-zone)
         (.format date)))))

(defn parse-datetime
  "Parse the given string to a Date object with the :datetime-format defined in *api-options*.
  NOTE: The system's default time zone is used when not provided."
  ([^String s] (parse-datetime s nil))
  ([^String s ^String time-zone]
   (let [{:keys [datetime-format]} *api-context*]
     (-> (make-date-format datetime-format time-zone)
         (.parse s)))))

(defn param-to-str [param]
  "Format the given parameter value to string."
  (cond
    (instance? Date param) (format-datetime param)
    (sequential? param) (str/join "," param)
    :else (str param)))

(defn make-url
  "Make full URL by adding base URL and filling path parameters."
  [path path-params]
  (let [path (reduce (fn [p [k v]]
                       (str/replace p (re-pattern (str "\\{" k "\\}")) (param-to-str v)))
                     path
                     path-params)]
    (str (:base-url *api-context*) path)))

(defn normalize-param
  "Normalize parameter value, handling three cases:
  for sequential value, normalize each elements of it;
  for File value, do nothing with it;
  otherwise, call `param-to-string`."
  [param]
  (cond
    (sequential? param) (map normalize-param param)
    (instance? File param) param
    :else (param-to-str param)))

(defn normalize-params
  "Normalize parameters values: remove nils, format to string with `param-to-str`."
  [params]
  (->> params
       (remove (comp nil? second))
       (map (fn [[k v]] [k (normalize-param v)]))
       (into {})))

(defn json-mime? [mime]
  "Check if the given MIME is a standard JSON MIME or :json."
  (if mime
    (or (= :json mime)
        (re-matches #"application/json(;.*)?" (name mime)))))

(defn json-preferred-mime [mimes]
  "Choose a MIME from the given MIMEs with JSON preferred,
  i.e. return JSON if included, otherwise return the first one."
  (-> (filter json-mime? mimes)
      first
      (or (first mimes))))

(defn serialize
  "Serialize the given data according to content-type.
  Only JSON is supported for now."
  [data content-type]
  (if (json-mime? content-type)
    (generate-string data {:date-format (:datetime-format *api-context*)})
    (throw (IllegalArgumentException. (str "Content type \"" content-type "\" is not support for serialization")))))

(defn deserialize
  "Deserialize the given HTTP response according to the Content-Type header."
  [{:keys [body] {:keys [content-type]} :headers}]
  (cond
    (json-mime? content-type)
    (try
      (parse-string body true)
      (catch JsonParseException e
        ;; return the body string directly on JSON parsing error
        body))
    ;; for non-JSON response, return the body string directly
    :else body))

(defn form-params-to-multipart
  "Convert the given form parameters map into a vector as clj-http's :multipart option."
  [form-params]
  (->> form-params
       (map (fn [[k v]] (array-map :name k :content v)))
       vec))

(defn call-api
  "Call an API by making HTTP request and return its response."
  [path method {:keys [path-params query-params header-params form-params body-param content-types accepts]}]
  (let [{:keys [debug]} *api-context*
        url (make-url path path-params)
        content-type (or (json-preferred-mime content-types)
                         (and body-param :json))
        accept (or (json-preferred-mime accepts) :json)
        multipart? (= "multipart/form-data" content-type)
        opts (cond-> {:url url :method method}
                     accept (assoc :accept accept)
                     (seq query-params) (assoc :query-params (normalize-params query-params))
                     (seq header-params) (assoc :header-params (normalize-params header-params))
                     (and content-type (not multipart?)) (assoc :content-type content-type)
                     multipart? (assoc :multipart (-> form-params
                                                      normalize-params
                                                      form-params-to-multipart))
                     (and (not multipart?) (seq form-params)) (assoc :form-params (normalize-params form-params))
                     body-param (assoc :body (serialize body-param content-type))
                     debug (assoc :debug true :debug-body true))
        resp (client/request opts)]
    (when debug
      (println "Response:")
      (println resp))
    (deserialize resp)))

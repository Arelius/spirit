(ns spirit.jade
  (:require
   [spirit.config :as conf]
   [spirit.xml]
   [org.danlarkin (json :as json)]
   [clojure.contrib.lazy-xml :as xml]
   [clojure.contrib.string :as string])
  (:use
   ring.adapter.jetty
   ring.middleware.reload
   ring.middleware.stacktrace
   clout.core
   clojure.string)
  (:import (org.mortbay.jetty.security SslSocketConnector)))

;; Util functions, put me in utils!
(defn find-first [pred col]
  (let [ret (pred (first col))]
    (if ret
      ret
      (if (empty? (next col))
        false
        (recur pred (next col))))))

(def route-handlers
  (atom ()))

(defn filter-params [param-list]
  (filter symbol? param-list))

(defn generate-url [namespace params]
  (apply str
         (cons
          "/"
          (concat
           (interpose
            "/"
            (concat
             (split (name namespace) #"\.")
             (map
              (fn [param]
                (str ":"
                     (name param)))
              (filter-params params))))))))

(defn build-handler-docs [name url doc-description return-description verb-list auth param-list]
  {:api-name name
   :api-url url
   :api-description doc-description
   :return-description return-description
   :param-descriptions (apply hash-map
                              (map
                               (fn [s]
                                 (if (symbol? s)
                                   (keyword s)
                                   s))
                               param-list))
   :accepted-verbs verb-list
   :required-authentication auth})

(defn get-auth [request]
  (if (= (request :scheme)
         :https)
    :auth-private
    :auth-public))

(defn compare-auth [request compare]
  (or (= compare :auth-public)
      (= (get-auth request) compare)))

(defn private-auth? [request]
  (= (get-auth request) :auth-private))

(defn create-route-handler [handler-desc doc-description return-description verb-list auth param-list handler-fun]
  (let
      [name (if (list? handler-desc)
              (first handler-desc)
              handler-desc)
       match (if (list? handler-desc)
               (fnext handler-desc)
               (generate-url name param-list))]
    {:name name
     :match match
     :ext-match (str match ".:ext")
     :handler handler-fun
     :auth auth ;; Check if valid!
     :doc-info (build-handler-docs
                name match doc-description return-description verb-list auth param-list)}))

;; Currentlly will remove all matches with the same match string, regardless of verbs
(defn add-route-handler [handler]
  (swap! route-handlers
         (fn [handlers] (cons handler
                              (filter
                               (fn [h]
                                 (not= (h :name) (handler :name)))
                               handlers)))))

(defmacro define-web-api [handler-desc doc-description return-description verb-list auth params & body]
  (let* [handler (if (symbol? handler-desc)
                   handler-desc
                   `~handler-desc)
         param-list (seq params)
         params-var (gensym)
         param-map (vec
                    (apply concat
                           (map
                            (fn [param]
                              `(~param (~params-var ~(keyword param))))
                            (rest (filter-params param-list)))))]
        `(add-route-handler
          (create-route-handler ~handler ~doc-description ~return-description '~verb-list ~auth '~(rest param-list)
                                (fn [~(first param-list) ~params-var]
                                  (let ~param-map
                                    ~@body))))))


(define-web-api :spirit.list
   "Lists avaliable api calls."
   "List of avaliable apis."
   (:get)
   :auth-public
   [req]
   (filter
    (fn [x] x) ;; not false
    (map
     (fn [handler]
       (if (compare-auth req (handler :auth))
         {:api (string/replace-char \. \: (name (handler :name)))
          :url (handler :match)}
         false))
     (deref route-handlers))))

(define-web-api :spirit.help
  "Prints documentation for an api call."
  "Api documentation."
  (:get)
  :auth-public
  [req api-call "Api function to lookup help for."]
  (find-first
   (fn [handler]
     (if (compare-auth req (handler :auth))
       (if (= (keyword (string/replace-char \: \. api-call)) (handler :name))
         (handler :doc-info))
      false))
   (deref route-handlers)))

(define-web-api :spirit.hello
  "Says Hi"
  "The phrase 'Hello, [Name]'"
  (:get)
  :auth-public
  [req name "Name to say hello too."]
  (str "Hello, " name))

(comment
 (create-route-handler :spirit.users.user
                       "Yay!"
                       "Returns user map."
                       '(:get)
                       :auth-private
                       '(user-name "Name of User")
                       (fn [req params]
                         (params :user-name))))

(comment
 (define-web-api
   "Returns a user given the name."
   (:spirit.user "/spirit/user/:user-name") (:get) :auth-private [user-name]))

(defn json-safe-encode [response]
  (json/encode (if (or (seq? response)
                       (vector? response))
                 {:d response}
                 response)))

(defn protocol-conversion [response ext]
  (case ext
        "json" (json-safe-encode response)
        "clj" (str (if (= clojure.lang.LazySeq (class response))
                     (seq response)
                     response))
        "xml" (spirit.xml/render-xml response)
        (json-safe-encode response)))

(defn handler [req]
  (let*
      [match
       (find-first (fn [handler]
                     (let [match
                           (or
                            (route-matches (handler :ext-match) req)
                            (route-matches (handler :match) req))]
                       (if match
                         {:handler handler
                          :match match}
                         false)))
                   (deref route-handlers))
       ret (if match
             (((match :handler) :handler)
              req
              (match :match))
             false)]
    (if ret
      {:status  200
       :headers {"Content-Type" "text/xml"}
       :body    (protocol-conversion ret ((match :match) :ext))}
      {:status  404
       :headers {"Content-Type" "text/html"}
       :body    "Resource Not Found."})))

(def app
  (-> #'handler
      ;;(wrap-reload '(spirit.jade spirit.core spirit.config))
      (wrap-stacktrace)))

(defn boot-jade []
  (run-jetty app {:configurator
                  (fn [server]
                    ;; Taken from jerry.clj, so we can add setNeedClientAuth
                    (let [ssl-connector (SslSocketConnector.)]
                      (doto ssl-connector
                        (.setPort conf/ssl-service-port)
                        (.setKeystore conf/ssl-keystore)
                        (.setKeyPassword conf/ssl-key-password)
                        (.setTruststore conf/ssl-truststore)
                        (.setTrustPassword conf/ssl-trust-password)
                        (.setNeedClientAuth true))
                      (.addConnector server ssl-connector))
                    (def jade-server server))
                  :port conf/service-port}))


;; (boot-jade)

;; (.stop jade-server)

(ns spirit.jade
  (:require
   [spirit.config :as conf])
  (:use
   ring.adapter.jetty
   ring.middleware.reload
   ring.middleware.stacktrace
   clout.core
   clojure.string))

;; Util functions, put me in utils!
(defn find-first [pred col]
  (let [ret (pred (first col))]
    (if ret
      ret
      (if (empty? (next col))
        false
        (recur pred (next col))))))

(def route-handlers
  (list
   {:match 
    "/spirit/user/:user"
    :handler
    (fn [req params]
      (params :user))}))

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
   :acceptted-verbs verb-list
   :required-authentication auth})

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
     :doc-info (build-handler-docs
                name match doc-description return-description verb-list auth param-list)}))

(create-route-handler :spirit.users.user
                      "Yay!"
                      "Returns user map."
                      '(:get)
                      :auth-private
                      '(user-name "Name of User")
                      (fn [req params]
                        (params :user-name)))


(route-matches (generate-url :spirit.users.user '(user-name)))

;;Temp/test
(defn handler [req]
  (let
      [ret
       ((route-matches (str (generate-url :spirit.users.user '(user-name)) ".:ext") req) :ext)]
    (if ret
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    ret}
      {:status  404
       :headers {"Content-Type" "text/html"}
       :body    "Resource Not Found."})))

;; Autogenerate URL if missing, auth-private default.
(define-web-api
  "Returns a user given the name."
  (:spirit.user "/spirit/user/:user-name") (:get) :auth-private [user-name])

(defn handler [req]
  (let
      [ret
       (find-first (fn [handler]
                     (let [match
                           (route-matches (handler :match) req)]
                       (if match
                         ((handler :handler)
                          req
                          match)
                         false)))
                   route-handlers)]
    (if ret
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    ret}
      {:status  404
       :headers {"Content-Type" "text/html"}
       :body    "Resource Not Found."})))

(def app
  (-> #'handler
      ;;(wrap-reload '(spirit.jade spirit.core spirit.config))
      (wrap-stacktrace)))

(defn boot-jade []
  (run-jetty app {:port conf/service-port}))

(boot-jade)
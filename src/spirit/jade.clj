(ns spirit.jade
  (:require
   [spirit.config :as conf])
  (:use
   ring.adapter.jetty
   ring.middleware.reload
   ring.middleware.stacktrace
   clout.core))

(def route-handlers
  (list
   {:match 
    "/spirit/user/:user"
    :handler
    (fn [req params]
      (params :user))}))

;; Because filter returns a lazy list this should just run pred on the first one.
(defn find-first [pred col]
  (first (filter pred col)))

;; Need to return run on first!
(find-first (fn [x] (apply (fnext x) '(1 2))) route-handlers)

(defn handler [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body
   (find-first (fn [handler]
                 ((fnext handler)
                  req
                  (route-matches (first handler) req)))
               route-handlers)})

(map (fn [x] (fnext x)) '((1 2) (3 4)))

(map (fn [x y] ) {"one" "two" "three" "four"})

(map (fn [x] 2) '(1 2 3))

(def app
  (-> #'handler
      ;;(wrap-reload '(spirit.jade spirit.core spirit.config))
      (wrap-stacktrace)))

(defn boot-jade []
  (run-jetty app {:port conf/service-port}))

(boot-jade)
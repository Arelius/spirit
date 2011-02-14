(ns spirit.xml
  (use
   clojure.string))

;; Implement me!
(defn clense-str [node]
  node)

(defn wrap-node [type node key]
  (str  "<val type=\""
       (name type)
       "\""
       (if key
         (str " key=\""
              key
              "\"")
         "")
       ">"
       (clense-str node)
       "</val>"))



(defn render-node [node & keyl]
  (let [key (if keyl (first keyl))]
   (condp (fn [f x] (f node)) node
     string? (wrap-node 'string node key)
     keyword? (wrap-node 'keyword (name node) key)
     symbol? (wrap-node 'symbol (name node) key)
     number? (wrap-node 'number (str node) key)
     seq? (let
              [ret
               (apply str
                      (interpose
                       \space
                       (map
                        render-node
                        node)))]
            (if key
              (wrap-node 'list ret key)
              ret))
     map? (apply str
                 (wrap-node 'hash
                            (apply str
                                   (map
                                    (fn [x] (render-node (fnext x) (first x)))
                                    node))
                            key))
     "")))

(defn render-xml [graph]
  (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
   "<data>" (render-node graph) "</data>"))

(ns spirit.prequel
  (:require
   [clojure.contrib.sql :as sql]))

;; IS NOT
(def binary-operators
  '[|| * / & + - << >> & | < <= > >= = == != <> is in like glob match regexp and or]) ;; IS IN LIKE GLOB MATCH REGEXP AND OR

(def prefix-operators
  '[- + ~ not]) ;; NOT

(def functions
  '[upper lower]) ;; etc etc...

(def statements
  '[alter begin commit create delete drop end insert replace rollback select update])

(defn paren-wrap [expr]
  (str \(
       expr
       \)))

(defn str-expr [expr]
  (apply str (interpose \space expr)))

(defn gen-syntax [atom]
  (name atom))

(defn gen-atom [atom]
  (condp (fn [f x] (f atom)) atom
    keyword? (gen-syntax atom)
    symbol? \?
    atom))

;; better conversion needed then (symbol (name ...))
(defn statement? [expr]
  (or
   (and (symbol? expr)
        (some #{expr} statements))
   (and (keyword? expr)
        (some #{(symbol (name expr))} statements))))

(declare gen-expr)

(defn gen-simple-expr [expr]
  (str-expr
   (if (statement? (first expr))
     (cons
      (gen-syntax (first expr))
      (map
       gen-expr
       (rest expr)))
     (interpose
      \,
      (map
       gen-expr
       expr)))))

(defn paren-wrap-expr [expr]
  (let [ret (gen-expr expr)]
    (paren-wrap ret)))

;; interpose doesn't work great for every op.
(defn gen-binary-op [op expr]
  (str-expr
   (interpose
    (str op)
    (map
     (fn [e]
       (gen-expr e))
     expr))))

(defn gen-function [fun expr]
  (apply str
         (concat
          (list fun)
          (paren-wrap           
           (gen-simple-expr expr)))))

(defn gen-query-string [expr]
  (str
   (gen-simple-expr expr)
   \;))

;; Need to support strings, NIL!!
(defn gen-expr [expr]
  (if (seq? expr)
    (if (empty? (rest expr))
      (gen-atom expr)
     (condp (fn [f x] (some #{(first expr)} f)) (first expr)
       binary-operators (paren-wrap-expr (gen-binary-op (first expr) (rest expr)))
       prefix-operators (paren-wrap-expr (gen-simple-expr expr))
       functions (gen-function (first expr) (rest expr))
       (paren-wrap-expr (gen-simple-expr expr))))
    (gen-atom expr)))

(defn syntax-symbol? [expr]
  (and
   (not (seq? expr))
   (or
    (some #{expr}
          binary-operators)
    (some #{expr}
          prefix-operators)
    (some #{expr}
          functions)
    (statement? expr))))

(defn get-query-variables [expr]
  (if (seq? expr)
    (let [ex (if (syntax-symbol? (first expr))
               (rest expr)
               expr)]
      (apply concat
             (filter (fn [x] (not
                              (or
                               (nil? x)
                               (and (seq? x)
                                    (empty? x)))))
                     (map
                      get-query-variables
                      ex))))
    (if (symbol? expr)
      (list expr)
      nil)))

;; Evaluate using some sort of prepared statement cache.

(defmacro with-query-results [db res query & body]
  `(sql/with-connection ~db
    (sql/with-query-results ~res
      ~(vec
        (concat
         (list
          (gen-query-string query))
         (get-query-variables query)))
      ~@body)))

;; Watchout, this will resolve all results.
(defmacro query-all-results [db query]
  `(let [ret# (atom nil)]
     (with-query-results ~db res# ~query
       (swap!
        ret#
        (fn [r#] (doall res#))))
     (deref ret#)))

(defmacro query-first-result [db query]
  `(let [ret# (atom nil)]
     (with-query-results ~db res# ~query
       (swap!
        ret#
        (fn [r#] (first res#))))
     (deref ret#)))

(defmacro exec-command [db cmd]
  `(sql/with-connection ~db
     (sql/do-prepared
      ~@(concat
        (list (gen-query-string cmd))
        `((list ~@(get-query-variables cmd)))))))

;; Do something about these commas!
;; Is commaing a list the default?
(defmacro insert [db table values]
  `(exec-command ~db
                 (:insert :into ~(keyword table)
                          (~@(map
                              (fn [k]
                                k)
                              (keys values)))
                          :values
                          (~@(map
                              (fn [v]
                                v)
                              (vals values))))))

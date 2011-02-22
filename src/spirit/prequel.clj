(ns spirit.prequel
  (:use
   clojure.string))

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

(defn gen-simple-expr [expr]
  (str-expr
   (cons
    (if (some #{(first expr)} statements)
      (gen-syntax (first expr))
      (gen-expr (first expr)))
    (map
     gen-expr
     (rest expr)))))

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
    (some #{expr}
          statements))))

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

(get-query-variables '(select :* :from :users :where (= (lower x) (select :* :from :blah :where (= x y)))))

(gen-query-string '(select :* :from :users :where (= (lower :users.login) (lower ?))))

(gen-query-string '(select :* :from :users :where (and (= (lower :users.login) (select :* :from :x)) (= 3 (+ 1 (- 2 3))))))

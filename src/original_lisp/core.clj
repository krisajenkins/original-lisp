(ns original-lisp.core)

; TODO A nasty hack to circumvent the circular dependency. What's the official way to handle this?
(def l-evcon)
(def l-evlis)

(defn atom? [x]
  (or (not (seq? x))
      (empty? x)))

(defn null? [x]
  (and (seq? x)
       (empty? x)))

(defn pair
  [xs ys]
  {:pre [(= (count xs) (count ys))]}
  (map list xs ys))

; Some aliases.
(def append concat)

; NOTE - There seems to be a bug in Graham's paper. His version of assoc does not allow for x-not-found.
(defn l-assoc
  [x ys]
  (cond
    (= x (first (first ys))) (first (rest (first ys)))
    (empty? ys) nil
    :else (recur x (rest ys))))

(defn l-eval [expr env]
  (cond
    (atom? expr) (l-assoc expr env)

    (atom? (first expr)) (cond
                           (= (first expr) 'quote) (first (rest expr))
                           (= (first expr) 'atom)  (atom? (l-eval (first (rest expr)) env))
                           (= (first expr) 'eq)    (= (l-eval (first (rest expr)) env)
                                                      (l-eval (first (rest (rest expr))) env))
                           (= (first expr) 'car)   (first (l-eval (first (rest expr)) env))
                           (= (first expr) 'cdr)   (rest (l-eval (first (rest expr)) env))
                           (= (first expr) 'cons)  (cons (l-eval (first (rest expr)) env)
                                                         (l-eval (first (rest (rest expr))) env))
                           (= (first expr) 'cond)  (l-evcon (rest expr) env)
                           :else                   (l-eval (cons (l-assoc (first expr) env) (rest expr))
                                                           env))

    (= (first (first expr)) 'label)  (l-eval (cons (first (rest (rest (first expr))))
                                                   (rest expr))
                                             (cons (list (first (rest (first expr)))
                                                         (first expr))
                                                   env))

    (= (first (first expr)) 'lambda) (l-eval (first (rest (rest (first expr))))
                                             (append (pair (first (rest (first expr))) (l-evlis (rest expr) env))
                                                     env))))

(defn l-evcon [condition env]
  (cond
    (l-eval (first (first condition)) env) (l-eval (first (rest (first condition))) env)
    :else (recur (rest condition) env)))

(defn l-evlis [m env]
  (cond
    (null? m) '()
    :else (cons (l-eval (first m) env)
                (l-evlis (rest m) env))))

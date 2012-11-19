(ns original-lisp.core)

(defn atom? [x]
  (or (not (seq? x))
      (empty? x)))

(defn l-eval [expr env]
  (cond
    (atom? expr) (env expr)
    (atom? (first expr)) (cond
                           (= (first expr) 'quote) (first (rest expr))
                           (= (first expr) 'atom) (atom? (l-eval (first (rest expr)) env))
                           (= (first expr) 'eq) (= (l-eval (first (rest expr)) env)
                                                   (l-eval (first (rest (rest expr))) env))
                           (= (first expr) 'car) (first (l-eval (first (rest expr)) env))
                           (= (first expr) 'cdr) (rest (l-eval (first (rest expr)) env))
                           (= (first expr) 'cons) (cons (l-eval (first (rest expr)) env)
                                                        (l-eval (first (rest (rest expr))) env)))))

(defn l-evcon [c a]
  )

(defn l-evlis [m a]
  )

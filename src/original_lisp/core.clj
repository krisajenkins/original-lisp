(ns original-lisp.core)

(defn atom? [x]
  (not (seq? x)))

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
                                                        (l-eval (first (rest (rest expr))) env)
                                                        )
                           )
    )
  )

(l-eval '(quote (this that)) {'this 'five 'that 'ten})
(l-eval '(car (this that)) {'this 'five 'that 'ten})

(l-eval '(car ((quote this) (quote that))) {'this 'five 'that 'ten})
(l-eval '(car (this that)) {'this 'five 'that 'ten})

(l-eval '(car ((quote this) (quote that))) {'this 'five 'that 'ten})
(first (first (rest '(car (this that)))))


(defn l-evcon [c a]

  )

(defn l-evlis [m a]

  )

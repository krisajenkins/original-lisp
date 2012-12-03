(ns original-lisp.core)

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

(defn third [x]
  (second (rest x)))

; NOTE - There seems to be a bug in Graham's paper. His version of assoc does
;   not allow for x-not-found. (Though it could be that I just don't understand
;   Common Lisp's cond).
(defn l-assoc
  [x ys]
  (cond
    (empty? ys) nil
    (= x (first (first ys))) (second (first ys))
    :else (recur x (rest ys))))

; TODO A nasty hack to circumvent the circular dependency. What's the official way to handle this?
(def l-evcond)
(def l-evlis)

(defn l-eval [expr env]
  (cond
    (atom? expr) (l-assoc expr env)

    (atom? (first expr)) (cond
                           (= (first expr) 'quote) (second expr)
                           (= (first expr) 'atom)  (atom? (l-eval (second expr) env))
                           (= (first expr) 'eq)    (= (l-eval (second expr) env)
                                                      (l-eval (third expr) env))
                           (= (first expr) 'car)   (first (l-eval (second expr) env))
                           (= (first expr) 'cdr)   (rest (l-eval (second expr) env))
                           (= (first expr) 'cons)  (cons (l-eval (second expr) env)
                                                         (l-eval (third expr) env))
                           (= (first expr) 'cond)  (l-evcond (rest expr) env)
                           :else                   (l-eval (cons (l-assoc (first expr) env)
                                                                 (rest expr))
                                                           env))

    (= (first (first expr)) 'label)  (let [[_ expr-name sub-expr] (first expr)]
                                       (l-eval (cons sub-expr (rest expr))
                                               (cons (list expr-name (first expr))
                                                     env)))

    (= (first (first expr)) 'lambda) (let [[_ bindings sub-expr] (first expr)]
                                       (l-eval sub-expr
                                               (append (pair bindings
                                                             (l-evlis (rest expr) env))
                                                       env)))))

(defn l-evcond [[condition & conditions] env]
  (cond
    (l-eval (first condition) env) (l-eval (second condition) env)
    :else (recur conditions env)))

(defn l-evlis [m env]
  (cond
    (null? m) '()
    :else (cons (l-eval (first m) env)
                (l-evlis (rest m) env))))

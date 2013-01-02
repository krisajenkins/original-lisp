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

(declare l-evcond l-evlis)

(defn l-eval [expr env]
  (cond
    (atom? expr) (l-assoc expr env)

    (atom? (first expr)) (case (first expr)
                           quote  (second expr)
                           atom   (atom? (l-eval (second expr) env))
                           car    (first (l-eval (second expr) env))
                           cdr    (rest (l-eval (second expr) env))
                           cond   (l-evcond (rest expr) env)
                           cons   (cons (l-eval (second expr) env)
                                        (l-eval (third expr) env))
                           eq     (= (l-eval (second expr) env)
                                     (l-eval (third expr) env))
                           (l-eval (cons (l-assoc (first expr) env)
                                         (rest expr))
                                   env))

    :else (case (first (first expr))
            label (let [[_ expr-name sub-expr] (first expr)]
                    (l-eval (cons sub-expr (rest expr))
                            (cons (list expr-name (first expr))
                                  env)))

            lambda (let [[_ bindings sub-expr] (first expr)]
                     (l-eval sub-expr
                             (append (pair bindings
                                           (l-evlis (rest expr) env))
                                     env))))))

(defn l-evcond [[condition & conditions] env]
  (if (l-eval (first condition) env)
    (l-eval (second condition) env)
    (recur conditions env)))

(defn l-evlis [m env]
  (if (null? m)
    '()
    (cons (l-eval (first m) env)
          (l-evlis (rest m) env))))

(ns original-lisp.test.core
  (:use clojure.test
        original-lisp.core))

(deftest atom?-test
  (is (atom? 'a))
  (is (atom? '()))
  (is (not (atom? '(1 2 3)))))

(deftest atom-test
  (is (= (l-eval 'a {'a 5})
         5)))

(deftest quote-test
  (is (= (l-eval '(quote thing) {})
         'thing
         )))

(deftest atom-symbol-test
  (is (= (l-eval '(atom thing) {'thing "THING"})
         true))
  (is (= (l-eval '(atom thing) {'thing '(1 2 3)})
         false)))

(deftest eq-test
  (is (= (l-eval '(eq this that) {'this 5 'that 5})
         true))
  (is (= (l-eval '(eq this that) {'this 5 'that 10})
         false)))

(deftest car-test
  (is (= (l-eval '(car (cons a (cons b ()))) {'a 1
                                              'b 2})
         1))
  )

(deftest cdr-test
  (is (= (l-eval '(cdr (cons a (cons b ()))) {'a 1
                                              'b 2})
         '(2)))
  )

(deftest cons-test
  (is (= (l-eval '(cons a ()) {'a 1})
         '(1)))
  (is (= (l-eval '(cons a (cons b ())) {'a 1
                                        'b 2})
         '(1 2)))
  )

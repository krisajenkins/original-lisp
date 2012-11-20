(ns original-lisp.test.core
  (:use clojure.test
        original-lisp.core))

(deftest atom?-test
  (is (atom? 'a))
  (is (atom? '()))
  (is (not (atom? '(1 2 3)))))

(deftest null?-test
  (is (not (null? 'a)))
  (is (null? '()))
  (is (not (null? '(1 2 3)))))

(deftest pair-test
  (is (= (pair '(a b c) '(x y z))
         '((a x) (b y) (c z))
         )))

(deftest append-test
  (is (= (append '(a b) '(x y))
         '(a b x y))))

(deftest l-assoc-test
  (is (= (l-assoc 'x '((x a) (y b)))
         'a))
  (is (= (l-assoc 'x '((x new) (x a) (y b)))
         'new))
  (is (= (l-assoc 'y '((x new) (x a) (y b)))
         'b)))

(deftest atom-test
  (is (= (l-eval 'a '((a 5) (b 10)))
         5))
  (is (= (l-eval 'b '((a 5) (b 10)))
         10)))

(deftest quote-test
  (is (= (l-eval '(quote thing) '())
         'thing)))

(deftest atom-symbol-test
  (is (= (l-eval '(atom thing) '((thing "THING")))
         true))
  (is (= (l-eval '(atom thing) '((thing '(1 2 3))))
         false)))

(deftest eq-test
  (is (= (l-eval '(eq this that) '((this 5) (that 5)))
         true))
  (is (= (l-eval '(eq this that) '((this 5) (that 10)))
         false)))

(deftest car-test
  (is (= (l-eval '(cons a (cons b '())) '((a 1) (b 2)))
         '(1 2)))
  (is (= (l-eval '(car (cons a (cons b '()))) '((a 1) (b 2)))
         1)))

(deftest cdr-test
  (is (= (l-eval '(cdr (cons a (cons b '()))) '((a 1) (b 2)))
         '(2))))

(deftest cons-test
  (is (= (l-eval '(cons a '()) '((a 1)))
         '(1)))
  (is (= (l-eval '(cons a (cons b '())) '((a 1) (b 2)))
         '(1 2))))

(deftest cond-test
  (is (= (l-eval '(cond (a b) (c d)) '((a 1) (b 2) (c 3) (d 4)))
         2))
  (is (= (l-eval '(cond (a b) (c d)) '((b 2) (c 3) (d 4)))
         4)))

(deftest substitute-head-test
  (is (= (l-eval '(woo thing) '((woo quote)))
         'thing)))

(deftest label-test
  (is (= (l-eval '((label firstatom (lambda (x)
                                            (cond ((atom x) x)
                                                  ('t (firstatom (car x))))))
                     y)
                 '((y ((a b) (c d)))))
         'a)))

(deftest lambda-test
  (is (= (l-eval '(f '(b c))
                 '((f (lambda (x) (cons 'a x)))))
         '(a b c))))

(deftest paul-graham-examples
  (is (= (l-eval 'x '((x a) (y b)))
         'a))

  (is (l-eval '(eq a a) '()))

  (is (= (l-eval '(cons x '(b c))
                 '((x a) (y b)))
         '(a b c)))

  (is (= (l-eval '(cond ((atom x) 'atom)
                        ('t 'list))
                 '((x (a b))))
         'list))

  (is (= (l-eval '(f '(b c))
                 '((f (lambda (x) (cons 'a x)))))
         '(a b c)))

  (is (= (l-eval '((lambda (x y) (cons x (cdr y)))
                     'a
                     '(b c d))
                 '())
        '(a c d))))

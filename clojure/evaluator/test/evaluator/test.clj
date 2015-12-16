(ns evaluator.test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [evaluator.core :refer [eval]]))

(deftest evaluation
  (let [env (atom {'+ +})]
    (is (= (eval env 0) 0))
    (is (= (eval env "foo") "foo"))
    (is (= (eval env '(quote (foo bar))) '(foo bar)))
    (is (= (eval env '(if false "foo" 0)) 0))
    (is (= (eval env '(begin (define bar "bar") bar)) "bar"))
    (eval env '(define double (lambda (x) (+ x x))))
    (eval env '(define foo (double (double 3))))
    (is (= (eval env 'foo) 12))))

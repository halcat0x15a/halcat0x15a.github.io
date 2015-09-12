(ns evaluator.test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [evaluator.core :refer [eval]]))

(deftest evaluation
  (let [env (atom {'+ +})]
    (eval env '(define double (lambda (x) (+ x x))))
    (eval env '(define foo (double (double 3))))
    (is (= (eval env 'foo) 12))))

(ns generic.test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer :all]
            [clojure.algo.generic.arithmetic :refer (+ - * / zero one)]
            [generic.complex :refer :all]))

(deftest arithmetic
  (is (= (+ 1 2) 3))
  (is (= (- 1 2) -1))
  (is (= (* 1 2) 2))
  (is (= (/ 1 2) 1/2))
  (is (= (+ 2 zero) 2))
  (is (= (- 3 zero) 3))
  (is (= (* 5 one) 5))
  (is (= (/ 7 one) 7))
  (is (= (+) zero))
  (is (= (*) one)))

(deftest complex
  (is (= (+ (->Complex 1 1) (->Complex 1 1)) (->Complex 2 2)))
  (is (= (+ (->Complex 1 1) 1) (->Complex 2 1)))
  (is (= (+ (->Complex 1 1) zero) (->Complex 1 1)))
  (is (= (- (->Complex 1 1) (->Complex 1 1)) (->Complex 0 0)))
  (is (= (- (->Complex 1 1) 1) (->Complex 0 1)))
  (is (= (- (->Complex 1 1) zero) (->Complex 1 1)))
  (is (= (* (->Complex 1 1) (->Complex 1 1)) (->Complex 0 2)))
  (is (= (* (->Complex 1 1) 0) (->Complex 0 0)))
  (is (= (* (->Complex 1 1) one) (->Complex 1 1)))
  (is (= (/ (->Complex 1 1) (->Complex 1 1)) (->Complex 1 0)))
  (is (= (/ (->Complex 1 1) (->Complex 0 1)) (->Complex 1 -1)))
  (is (= (/ (->Complex 1 1) one) (->Complex 1 1))))

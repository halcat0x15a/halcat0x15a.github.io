(ns exparser.test
  (:require [clojure.test :refer :all]
            [exparser.core :refer [eval']]))

(deftest evaluation
  (is (= (eval' '[2 + 3 * 5 - 7]) '(10)))
  (is (= (eval' '[2 - 4 + 8 * 16 / 32]) '(2))))

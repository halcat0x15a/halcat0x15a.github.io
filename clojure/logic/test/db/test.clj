(ns db.test
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [run* fresh ==]]
            [clojure.core.logic.pldb :refer [with-db]]
            [clojure.test :refer :all]
            [db.core :refer :all]))

(deftest query
  (is (= (with-db facts
           (run* [q]
             (fresh [a b]
               (language a)
               (jvm a)
               (logic b)
               (library a b)
               (== q [a b]))))
         '([Clojure core.logic]))))

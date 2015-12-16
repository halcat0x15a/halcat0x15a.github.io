(ns parser.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.dcg :refer :all]))

(def-->e number [n]
  ([_] [n] (!dcg (project [n] (== (number? n) true)))))

(def-->e term [t]
  ([_] (fresh [x y]
         (number x) '[*] (term y)
         (!dcg (project [x y] (== t (* y x))))))
  ([_] (fresh [x y]
         (number x) '[/] (term y)
         (!dcg (project [x y] (== t (/ y x))))))
  ([_] (number t)))

(def-->e expr [e]
  ([_] (fresh [x y]
         (term x) '[+] (expr y)
         (!dcg (project [x y] (== e (+ y x))))))
  ([_] (fresh [x y]
         (term x) '[-] (expr y)
         (!dcg (project [x y] (== e (- y x))))))
  ([_] (term e)))

(defn eval' [e]
  (run* [q]
    (expr q (reverse e) [])))

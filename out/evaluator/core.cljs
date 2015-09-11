(ns evaluator.core
  (:require [cljs.reader :as reader]))

(defn self-evaluating? [exp]
  (or (true? exp)
      (false? exp)
      (number? exp)
      (string? exp)))

(defmulti eval-form (fn [env exp] (first exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (get @env (munge (str exp)))
        (seq? exp) (eval-form env exp)))

(defmethod eval-form 'quote [env [_ quotation]] quotation)

(defmethod eval-form 'if [env [_ predicate consequent alternative]]
  (if (eval env predicate)
    (eval env consequent)
    (eval env alternative)))

(defmethod eval-form 'define [env [_ variable value]]
  (swap! env #(assoc % (str variable) (eval env value))))

(defmethod eval-form 'begin [env [& exps]]
  (->> exps (map #(eval env %)) last))

(defprotocol Procedure
  (app [f args]))

(defmethod eval-form :default [env [operator & operands]]
  (app (eval env operator) (map #(eval env %) operands)))

(deftype Lambda [env parameters body]
  Procedure
  (app [lambda args]
    (eval (atom (merge @env (zipmap parameters args))) body)))

(defmethod eval-form 'lambda [env [_ parameters body]]
  (Lambda. env parameters body))

(extend-protocol Procedure
  function
  (app [f args] (apply f args)))

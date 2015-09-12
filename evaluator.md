---
layout: default
title: Lisp on Clojure
---

# Lisp on Clojure

Clojureの特徴を簡単なLisp評価機を作る過程で紹介します.

今回作成する評価機は, 以下のような動作をします.

```clojure
(def env (atom {'+ +}))

(eval env '(define double (lambda (x) (+ x x))))

(eval env '(define foo (double (double 3))))

(assert (= (eval env 'foo) 12))
```

この例は`env`の元で`double`と`foo`を定義し, `assert`により`foo`の値が12であることを確認しています.

## Atom

Clojureは`def`により`var`を作成し, 現在の`namespace`に束縛します.

`var`に対する再代入は通常出来ません.

ここでは, 可変参照として`atom`を使用します.

```clojure
(def foo (atom 1))

(assert (= @foo 1))

(reset! foo 2)

(assert (= @foo 2))

(swap! foo inc)

(assert (= @foo 3))
```

`@`はAtomの現在の値を返します.

`reset!`を使うことで再代入が可能です.

## 自己評価式

文字列や数値など, リテラルのみを評価できる単純な評価機から作成します.

評価機は環境と式を引数にとり, 環境のもとで式を評価し値を返します.

```clojure
(defn self-evaluating? [exp]
  (or (true? exp)
      (false? exp)
      (number? exp)
      (string? exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp))

(assert (= (eval (atom {}) 100) 100))
(assert (= (eval (atom {}) "foo") "foo"))
```

## 変数の探索

環境は`hash-map`で表現します.

式がシンボルの場合はそれを変数として環境から値を参照します.

```clojure
(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (get @env exp)))

(assert (= (eval (atom {'foo 100}) 'foo)) 100)
```

## Multimethod

Clojureで多相的な関数を定義する方法のひとつにMultimethodが存在します.

`defmulti`でディスパッチ関数を定義し, `defmethod`により対応する値と手続きを記述します.

```clojure
(defmulti foo (fn [x] x))
(defmethod foo 'foo [x] 100)
(defmethod foo 'bar [x] "bar")
(defmethod foo :default [x] x)

(assert (= (foo 'foo) 100))
(assert (= (foo 'bar) "bar"))
(assert (= (foo 'baz) 'baz))
```

## 特殊形式

特殊形式の判別はリストの先頭を比較する必要があります.

Multimethodを用いることで`eval`を変更することなく制御構造を追加することができます.

```clojure
(defmulti eval-form (fn [env exp] (first exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (get @env exp)
        (seq? exp) (eval-form env exp)))
```

## quote

`quote`は引数の式を評価せずそのまま返します.

式の分解には分配束縛を用います.

```clojure
(defmethod eval-form 'quote [env [_ quotation]] quotation)

(assert (= (eval (atom {}) '(quote (foo bar))) '(foo bar)))
```

## if

`if`は`predicate`を評価し, 真ならば`consequent`を偽ならば`alternative`を評価します.

```clojure
(defmethod eval-form 'if [env [_ predicate consequent alternative]]
  (if (eval env predicate)
    (eval env consequent)
    (eval env alternative)))

(assert (= (eval (atom {}) '(if false "foo" 100)) 100))
```

## define

`define`は環境に変数を束縛します.

無名関数`(fn [x] (f x))`は`#(f %)`のように記述できます.

```clojure
(defmethod eval-form 'define [env [_ variable value]]
  (swap! env #(assoc % variable (eval env value))))

(let [env (atom {})]
  (eval env '(define foo 100))
  (assert (= (eval env 'foo) 100)))
```

## threading macro

Clojureでは`->`や`->>`を使うことでデータの流れがわかりやすくなる, ネストが減るなどの利点があります.

```clojure
(assert (= (-> [] (conj "foo") (conj "bar") first)
           (first (conj (conj [] "foo") "bar"))))

(assert (= (->> (range 10) (filter odd?) reverse)
           (reverse (filter odd? (range 10)))))
```

## begin

`begin`は引数の式を逐次評価します.

`&`を使うと複数の値を束縛出来ます.

```clojure
(defmethod eval-form 'begin [env [& exps]]
  (->> exps (map (partial eval env)) last))

(assert (= (eval (atom {}) '(begin (define bar "bar") bar)) "bar"))
```

## Protocol

ProtocolはClojureにおける抽象機構です.

TypeやRecordは定義時にProtocolを実装することができます.

```clojure
(defprotocol Foo
  (foo []))

(deftype Bar []
  Foo
  (foo [] "bar"))

(defrecord Baz []
  Foo
  (foo [] "baz"))

(assert (= (foo (Bar.)) "bar"))
(assert (= (foo (Baz.)) "baz"))
```

## 適用

特殊形式以外のシンボルは関数適用として働きます.

この評価機では２種類の関数が存在します.

* 評価機で定義した関数
* Clojureの関数

これらに対してProtocolを定義します.

```clojure
(defprotocol Procedure
  (app [f args]))

(defmethod eval-form :default [env [operator & operands]]
  (app (eval env operator) (map #(eval env %) operands)))
```

## lambda

`lambda`は無名関数を生成します.

仮引数と実引数のペアで環境を拡張し, 式を評価します.

```clojure
(deftype Lambda [env parameters body]
  Procedure
  (app [lambda args]
    (eval (atom (merge @env (zipmap parameters args))) body)))

(defmethod eval-form 'lambda [env [_ parameters body]]
  (Lambda. env parameters body))

(assert (= (eval (atom {}) '((lambda (x y) y) "foo" "bar")) "bar"))
```

## primitive function

`clojure.lang.IFn`はClojureの関数を表すインターフェースです.

`extend-protocol`により既存のデータ型に対してProtocolの実装が可能です.

```clojure
(extend-protocol Procedure
  clojure.lang.IFn
  (app [f args] (apply f args)))

(assert (= (eval (atom {'+ +}) '(+ 2 3)) 5))
```

これで評価機は完成です.

## ClojureScript

Clojureで書かれているのでJavaScriptにコンパイルが可能です.

JavaScriptで動作させるには次のように書き換える必要があります.

```clojure
(extend-protocol Procedure
  function
  (app [f args] (apply f args)))
```

以下に今回実装した評価機を示します.

<div>
  <div class="form-group">
    <label for="result" class="control-label">Result</label>
    <input type="text" class="form-control" id="result" readonly>
  </div>
  <div class="form-group">
    <label for="source" class="control-label">Input</label>
    <textarea id="source" class="form-control"></textarea>
  </div>
  <button class="btn btn-default" onclick="document.getElementById('result').value = evaluator.core.eval(cljs.core.atom(cljs.core.js__GT_clj(cljs.core)), cljs.reader.read_string(document.getElementById('source').value))">Eval</button>
</div>

<script src="out/goog/base.js" type="text/javascript"></script>
<script src="out/main.js" type="text/javascript"></script>
<script type="text/javascript">goog.require("evaluator.core");</script>

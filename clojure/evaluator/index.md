---
layout: article
title: Lisp on Clojure
---

# Lisp on Clojure

Clojureの特徴を簡単なLisp評価機を作る過程で紹介します。

作成する評価機は以下のようになります。

```clojure
(def env (atom {'+ +}))

(eval env '(define double (lambda (x) (+ x x))))

(eval env '(define foo (double (double 3))))

(assert (= (eval env 'foo) 12))
```

最初に環境 `env` を定義します。

これは `'+` をキーにして、足し算の関数 `+` を値に持ちます。

次に `env` のもとで関数 `double` と `foo` を定義しています。

`double` は引数 `x` をとり、2つの `x` に `+` を適用する関数です。

`foo` は `3` に `double` を2回適用しています。

最後に `assert` で環境 `env` のもとで `foo` の値が `12` であることを確認しています。

## Atom

Clojureは `def` により `var` を作成し、現在の `namespace` に値を束縛します。

通常 `var` に対する再代入は出来ません。

ここでは可変参照として `atom` を使用します。

```clojure
(def foo (atom 1))
(assert (= @foo 1))

(reset! foo 2)
(assert (= @foo 2))

(swap! foo inc)
(assert (= @foo 3))
```

`@` はAtomの現在の値を返します。

`reset!` を使うことで値の再代入が可能です。

`swap!` はAtomの値に関数を適用し、その結果で値を置き換えます。

## 自己評価式

文字列や数値など、リテラルのみを評価できる単純な評価機から作成します。

評価機は環境と式を引数にとり、環境のもとで式を評価し値を返します。

```clojure
(defn self-evaluating? [exp]
  (or (true? exp)
      (false? exp)
      (number? exp)
      (string? exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp))

(assert (= (eval (atom {}) 0) 0))
(assert (= (eval (atom {}) "foo") "foo"))
```

`0` を評価すると `0` が返り、`"foo"` を評価すると `"foo"` が返ります。

## 変数の探索

環境はマップで表現します。

式がシンボルの場合はそれを変数として環境から値を参照します。

```clojure
(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (get @env exp)))

(assert (= (eval (atom {'foo 0}) 'foo)) 0)
```

マップは `{key value}` で作成することができます。

マップから値を取り出すには `get` を使います。

環境 `{'foo 0}` のもとで `foo` を評価すると `0` が返ります。


## Multimethod

Clojureで多相的な関数を定義する方法のひとつにMultimethodが存在します。

`defmulti` でディスパッチ関数を定義し、`defmethod` により対応する値と手続きを記述します。

```clojure
(defmulti foo (fn [x] x))
(defmethod foo 'foo [x] 0)
(defmethod foo 'bar [x] "bar")
(defmethod foo :default [x] x)

(assert (= (foo 'foo) 0))
(assert (= (foo 'bar) "bar"))
(assert (= (foo 'baz) 'baz))
```

`foo` のディスパッチ関数は `(fn [x] x)` で、単に引数の値を使ってディスパッチします。

対応する値が存在しなかった場合、`:default` が呼ばれます。

## 特殊形式

特殊形式の判別はリストの先頭を比較する必要があります。

Multimethodを用いることで `eval` の定義を変更することなく制御構造を追加することができます。

```clojure
(defmulti eval-form (fn [env exp] (first exp)))

(defn eval [env exp]
  (cond (self-evaluating? exp) exp
        (symbol? exp) (get @env exp)
        (seq? exp) (eval-form env exp)))
```

`eval-form` は評価する式の先頭のシンボルを使ってディスパッチします。

これから特殊形式を定義するときには `defmethod` を使って、`eval-form` を拡張していきます。

## 分配束縛

`let` や `defn` などの束縛形式では値を分解し、束縛することができます。

```clojure
(assert (= (let [[fst _ _] '(1 2 3)] fst) 1))
(assert (= (let [[_ snd _] [2 4 6]] snd) 4))
(assert (= (let [[_ & rest] [1 3 5]] rest) [3 5]))
```

この例は次のように動作します。

1. `fst` に一番目の要素を束縛し返す
2. `snd` に二番目の要素を束縛し返す
3. `rest` に二番目から残りの要素全てを束縛し返す

この分配束縛を利用して式を分解し、評価していきます。

## quote

`quote` は引数の式を評価せずそのまま返します。

```clojure
(defmethod eval-form 'quote [env [_ quotation]] quotation)

(assert (= (eval (atom {}) '(quote (foo bar))) '(foo bar)))
```

`(quote (foo bar))` は引数の式 `(foo bar)` を返します。

## if

`if` は1つ目の引数 `predicate` を評価し、真ならば2つ目の引数 `consequent` を偽ならば3つ目の引数 `alternative` を評価します。

```clojure
(defmethod eval-form 'if [env [_ predicate consequent alternative]]
  (if (eval env predicate)
    (eval env consequent)
    (eval env alternative)))

(assert (= (eval (atom {}) '(if false "foo" 100)) 100))
```

`(if false "foo" 100)` は条件式が `false` を返すので、3つ目の引数である `100` を返します。

## define

`define` は環境に変数を束縛します。

```clojure
(defmethod eval-form 'define [env [_ variable value]]
  (swap! env #(assoc % variable (eval env value))))

(let [env (atom {})]
  (eval env '(define foo 0))
  (assert (= (eval env 'foo) 0)))
```

無名関数 `(fn [x] (f x))` は `#(f %)` のように記述することができます。

`(define foo 0)` を `env` のもとで評価したあと、`foo` を `env` のもとで評価すると `0` が返ります。

## Threading macro

Clojureでは `->` や `->>` を使うことでデータの流れがわかりやすくなる、ネストが減るなどの利点があります。

次の例では `->` と `->>` がどのように展開されるかを示します。

```clojure
(assert (= (-> [] (conj "foo") (conj "bar") first)
           (first (conj (conj [] "foo") "bar"))))

(assert (= (->> (range 10) (filter odd?) reverse)
           (reverse (filter odd? (range 10)))))
```

`->` と `->>` は1番目の引数の値に、2番目以降の式を適用していきます。

`->` は引数の最初に、`->>` 引数の最後に適用します。

## begin

`begin` は引数の式を逐次評価し、最後の式の値を返します。

```clojure
(defmethod eval-form 'begin [env [_ & exps]]
  (->> exps (map #(eval env %)) last))

(assert (= (eval (atom {}) '(begin (define bar "bar") bar)) "bar"))
```

分配束縛は `&` で引数の式をシーケンスとして受け取ります。

`(begin (define bar "bar") bar)` は1番目の引数の式 `(define bar "bar")` を評価し、最後の引数の式 `bar` の値 `"bar"` を全体の結果として返します。

## Protocol

ProtocolはClojureにおける抽象機構です。

TypeやRecordは定義時にProtocolを実装することができます。

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

ProtocolはJavaにおけるinterfaceと同等のものと捉えることができます。

この例では `Bar` における `foo` の実装は `"bar"` であり、`Baz` における `foo` の実装は `"baz"` です。

既存のデータ型に対してもProtocolは実装可能です。

## 関数適用

特殊形式以外のシンボルは関数適用として働きます。

この評価機では2種類の関数が存在します。

* 評価機で定義した関数
* Clojureの関数

これらに対してProtocolを定義します。

```clojure
(defprotocol Procedure
  (app [f args]))

(defmethod eval-form :default [env [operator & operands]]
  (app (eval env operator) (map #(eval env %) operands)))
```

特殊形式以外の式は引数の値をすべて評価して、`Procedure` として関数の適用が行われます。

## lambda

`lambda` は無名関数を生成します。

仮引数と実引数のペアで環境を拡張し、式を評価します。

```clojure
(deftype Lambda [env parameters body]
  Procedure
  (app [lambda args]
    (eval (atom (merge @env (zipmap parameters args))) body)))

(defmethod eval-form 'lambda [env [_ parameters body]]
  (Lambda. env parameters body))

(assert (= (eval (atom {}) '((lambda (x y) y) "foo" "bar")) "bar"))
```

`Lambda` は環境 `env` と仮引数 `parameters` と式 `body` を値にもちます。

`Procedure` の実装は、仮引数 `parameters` と実引数の `args` を `zipmap` で仮引数をキーにして実引数を値にもつマップを作り、環境 `env` と `merge` することで環境を拡張し、`body` を評価します。

`((lambda (x y) y) "foo" "bar"))` はまず、`(lambda (x y) y)` で引数に `(x y)` をとって `y` を返す関数を作り、`"foo" "bar"` に適用します。

これにより、`"foo"` は `x` に、`"bar"` は `y` に束縛され、結果として `"bar"` が返ります。

## Primitive function

`clojure.lang.IFn` はClojureの関数を表すインターフェースです。

`extend-protocol` により既存のデータ型に対してProtocolの実装が可能です。

```clojure
(extend-protocol Procedure
  clojure.lang.IFn
  (app [f args] (apply f args)))

(assert (= (eval (atom {'+ +}) '(+ 2 3)) 5))
```

`(+ 2 3)` はまず、環境から `+` を参照してClojureの `+` 関数を返します。

`+` は `clojure.lang.IFn` を実装しているので、関数の適用で `apply` 関数により引数 `2 3` に適用され、`5` が返ります。

これで評価機は完成です。

## ClojureScript

ClojureのコードはClojureScriptを使うことでJavaScriptにコンパイルが可能です。

JavaScriptで動作させるには次のように書き換える必要があります。

```clojure
(extend-protocol Procedure
  function
  (app [f args] (apply f args)))
```

`function` はClojureScriptにおいて関数の型を表す特殊なキーワードです。

以下に今回実装した評価機を示します。

<div>
  <div class="form-group">
    <label for="result">Result</label>
    <input type="text" class="form-control" id="result" readonly>
  </div>
  <div class="form-group">
    <label for="source">Input</label>
    <textarea id="source" class="form-control">(+ 1 1)</textarea>
  </div>
  <button class="btn btn-primary" onclick="document.getElementById('result').value = evaluator.core.eval(cljs.core.atom(cljs.core.js__GT_clj(cljs.core)), cljs.reader.read_string(document.getElementById('source').value))">Eval</button>
</div>

<script src="main.js" type="text/javascript"></script>

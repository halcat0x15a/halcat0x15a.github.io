---
layout: default
title: Lispでも論理プログラミングがしたい！
---

# Lispでも論理プログラミングがしたい！

core.logicはClojureの準標準ライブラリ群であるClojure Contribに含まれる, 論理プログラミングのためのライブラリです.

この記事では, core.logicを使ったClojureにおける論理プログラミングを紹介します.

## core.logic

このライブラリはClojureで制約論理プログラミングとProlog-likeな関係プログラミングを可能にします.

Lispの論理プログラミングライブラリといっても, 処理系毎に様々なライブラリが存在します.

core.logicはSchemeの論理プログラミングライブラリであるminiKanren, そしてその拡張であるcKanrenに基づき実装されています.

## append

Prologの述語append/3は結合, 差分, 組み合わせと複数の意味を持ち, 論理プログラミングの例としてよく取り上げられます.

core.logicによるappendの例をみてみましょう.

```clojure
(defne append [x y z]
  ([() y y])
  ([[h . t] _ [h . t']] (append t y t')))
```

Prologのコードとよく似ていることがわかりますね?

```prolog
append([], L, L).
append([H|T1], L, [H|T2]) :- append(T1, L, T2).
```

REPLで実行してみましょう.

```clojure
user> (run* [q]
        (append [1 2] [3 4] q))
((1 2 3 4))
user> (run* [q]
        (append [1 2] q [1 2 3 4]))
((3 4))
user> (run* [q]
        (fresh [a b]
          (== q [a b])
          (append a b [1 2 3 4])))
([() [1 2 3 4]]
 [(1) (2 3 4)]
 [(1 2) (3 4)]
 [(1 2 3) (4)]
 [(1 2 3 4) ()])
```

このように, Prologとは少し違った質問の仕方をします.

これらのコードについて詳しくみていきましょう.

## core.logicの基本

core.logicを使って論理プログラミングを行う上で, 幾つかの重要な制御構造が存在します.

### run

`run*`は1つの論理変数とその論理変数に対する制約を与え, 制約を充す値の全てをリストで返します.

しかし, 制約を充す値が多く存在する, または無限に存在する場合はプログラムが応答しなくなるでしょう.

そこで, `run`を使用することで探索を制限することができます.

```clojure
user> (run 2 [q]
        (fresh [a b]
          (== q [a b])
          (append a b [1 2 3 4])))
([() [1 2 3 4]] [(1) (2 3 4)])
```

### fresh

Prologは論理変数に大文字を使用し, 質問の結果に使用した論理変数とその値が表示されます.

core.logicでは`fresh`用いて論理変数を明示的に宣言する必要があります.

```clojure
(defne number [char int]
  ([\1 1]) ([\2 2]) ([\3 3])
  ([\4 4]) ([\5 5]) ([\6 6])
  ([\7 7]) ([\8 8]) ([\9 9])
  ([\0 0]))
```

```clojure
user> (run* [q]
        (fresh [c]
          (number c q)))
(1 2 3 4 5 6 7 8 9 0)
user> (run* [q]
        (fresh [i]
          (number q i)))
(\1 \2 \3 \4 \5 \6 \7 \8 \9 \0)
```

Prologとは違い, ゴールの内側のみ有効な変数を定義することが可能です.

値が未確定な論理変数は, 各結果ごとに _0, _1, _2 と表示されます.

### project

`project`は論理変数の値を取り出し, ゴールに適用します.

主にClojureの関数を適用するときに使用します.

```clojure
(defna factorial [m n]
  ([0 1])
  ([1 1])
  ([_ _] (fresh [x y]
           (project [m] (== x (dec m)))
           (factorial x y)
           (project [m y] (== n (* m y))))))
```

```clojure
user> (run 1 [q] (factorial 5 q))
(120)
```

もし, 論理変数に値が束縛されていない場合は, 論理変数がそのまま渡されます.

```clojure
user> (run 1 [q] (factorial q 5))
ClassCastException clojure.core.logic.LVar cannot be cast to java.lang.Number  clojure.lang.Numbers.dec (Numbers.java:118)
```

## 探索の制御

core.logicにはPrologのcutに相当するものは存在しません.

そのかわり, 3つの制御構造が存在します.

### conde

`conde`は複数の節をとります.

節の各ゴールが全て真になるように値を推論します.

```clojure
user> (run* [q]
        (conde [(== q :foo)]
               [(== q :bar)]))
(:foo :bar)
```

### conda

`conda`はSoft cutと呼ばれるもので, 上部の節が成功した後, その後の節は無視されます.

```clojure
user> (run* [q]
        (conda [(conde [(== q :foo)]
                       [(== q :bar)])]
               [(== q :baz)]))
(:foo :bar)
```

### condu

`condu`はCommitted choiceと呼ばれ, 節の最初の成功を結果とします.

```clojure
user> (run* [q]
        (condu [(conde [(== q :foo)]
                       [(== q :bar)])]
               [(== q :baz)]))
(:foo)
```

これら`conde`, `conda`, `condu`によってパターンマッチや述語定義の制御構造が定義されます.

<table class="table">
  <caption>各制御の対応</caption>
  <thead>
    <tr><th>制御構造</th><th>論理和</th><th>Soft cut</th><th>Committed Choice</th></tr>
  </thead>
  <tbody>
    <tr><td>条件分岐</td><td>conde</td><td>conda</td><td>condu</td></tr>
    <tr><td>パターンマッチ</td><td>matche</td><td>matcha</td><td>matchu</td></tr>
    <tr><td>述語定義</td><td>defne</td><td>defna</td><td>defnu</td></tr>
  </tbody>
</table>

これらの制御構造が扱えれば, 大抵の述語を定義することが可能でしょう.

## データベース

`clojure.core.logic.pldb`を使うことで関係データベースを扱えます.

`db-rel`により関係を定義し, `db`によってデータベースを構築します.

```clojure
(db-rel language p)
(db-rel jvm p)
(db-rel logic p)
(db-rel library p p')

(def facts
  (db [language 'Java]
      [language 'Prolog]
      [language 'Clojure]
      [language 'Scheme]
      [jvm 'Java]
      [jvm 'Clojure]
      [logic 'Prolog]
      [logic 'core.logic]
      [logic 'miniKanren]
      [library 'Clojure 'core.logic]
      [library 'Scheme 'miniKanren]))
```

定義した関係は他の述語と同じ様に使えます.

```clojure
user> (with-db facts
        (run* [q]
          (fresh [a b]
            (language a)
            (jvm a)
            (logic b)
            (library a b)
            (== q [a b]))))
([Clojure core.logic])
```

## 速度

core.logicの実行速度はProlog処理系と比べると遅いです.

フィボナッチ数を求めるプログラムで速度を比較してみます.

### core.logic

```clojure
(defna fib [n r]
  ([0 1])
  ([1 1])
  ([_ _] (fresh [x x' y y']
           (project [n] (== x (dec n)))
           (project [n] (== x' (- n 2)))
           (fib x y)
           (fib x' y')
           (project [y y'] (== r (+ y y'))))))
```

```clojure
user> (time (run* [q] (fib 25 q)))
"Elapsed time: 6165.90255 msecs"
(121393)
```

### SWI-Prolog

```prolog
fib(0, 1).
fib(1, 1).
fib(M, N) :-
  X1 is M - 1,
  X2 is M - 2,
  fib(X1, Y1),
  fib(X2, Y2),
  N is Y1 + Y2.
```

```prolog
?- time(fib(25, N)).
% 364,176 inferences, 3.614 CPU in 3.626 seconds (100% CPU, 100780 Lips)
N = 121393 .
```

この速度はTablingによって改善できます.

### Tabling

Tablingは述語をメモ化するための機能です.

先ほどのフィボナッチ数の例に適用します.

```clojure
(defn fib' [n r]
  ((tabled [n r]
     (matchu [n r]
       ([0 1])
       ([1 1])
       ([_ _] (fresh [x x' y y']
                (project [n] (== x (dec n)))
                (project [n] (== x' (- n 2)))
                (fib' x y)
                (fib' x' y')
                (project [y y'] (== r (+ y y')))))))
   n r))
```

```clojure
user> (time (run 1 [q] (fib' 25 q)))
"Elapsed time: 59.461846 msecs"
(121393)
```

このように, Tablingが有効な計算では著しい速度の改善が認められます.

## DCG

Prologには限定節文法(Definite Clause Grammar)と呼ばれる構文解析のための機能があり, core.logicにも`clojure.core.logic.dcg`としてDCGが実験的に実装されています.

```clojure
(def-->e tuple [q]
  ([[[:fst a] [:snd b]]] [a] [b]))
```

この構文は以下の述語と同等の意味を持ちます.

```clojure
(defne tuple' [q x y]
  ([[[:fst a] [:snd b]] [a b . c] c]))
```

```clojure
user> (run* [q]
        (fresh [a b]
          (== q [a b])
          (tuple a '[foo bar baz] b)))
([[[:fst foo] [:snd bar]] (baz)])
user> (run* [q]
        (fresh [a b]
          (== q [a b])
          (tuple' a '[foo bar baz] b)))
([[[:fst foo] [:snd bar]] (baz)])
```

この構文は述語定義のシンタックスシュガーであり, 暗黙的に2つの引数を取っています.

さらに, この構文の中で呼び出される関数にも暗黙的に2の値が渡されるため, 通常の関数を呼ぶためには`!dcg`を使う必要があります.

この構文を用いて, 数式の解析, 計算をする例を作ります.

```clojure
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
```

同じ優先度の演算が並んだ場合の計算順序を正しくするため, 入力を反転し, オペランドの位置も反転させています.

```clojure
user> (eval' '[2 + 3 * 5 - 7])
(10)
user> (eval' '[2 - 4 + 8 * 16 / 32])
(2)
```

`fresh`はこの構文の中で特別扱いされているため, 通常の制御構造と変わりなく呼び出すことが可能です.

## ClojureScript

ClojureScriptはClojureのコードをJavaScriptへ変換するコンパイラです.

core.logicにはClojureScript版のライブラリも存在し, 論理プログラミングがJavaScript上で可能です.

## まとめ

core.logicには基本的な論理プログラミングの機能だけでなく, TablingやDCGなどの実用的な機能も兼ね備えています.

ライブラリとして実装され, JVMやJavaScript上で動作するので, サーバーやデスクトップ, Web上で動作するプログラムに簡単に組込むことが可能です.

また, Clojureで論理プログラミングを簡素な構文で実現するために, 多くの機能がマクロによって定義されています.

それ故に, エラーが追い辛く, Clojureの他のマクロと組合せ難いという欠点があります.

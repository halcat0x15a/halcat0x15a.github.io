---
layout: default
title: Clojure Contrib
---

# Clojure Contrib

Clojureの準標準的なライブラリ群であるContribライブラリを紹介します.

# core.async

core.asyncは非同期プログラミングをサポートします.

## チャネル

core.asyncではチャネルを用いて値をやりとりします.

チャネルの生成には`chan`を使い, チャネルに値を書き込むには`>!!`, 読み込むには`<!!`を使います.

これらの関数は操作が完了するまでスレッドをブロックすることから, 通常`future`などと組み合わせて使います.

```clojure
(require '[clojure.core.async :refer [chan <!! >!!]])

(let [c (chan)
      r (future (<!! c))]
  (>!! c "hello")
  (assert (= @r "hello")))
```

上記のコードでは, `future`で非同期にチャネルから値を読み込み, チャネルに値を書き込んだ後, `future`から値を取得します.

`timeout`は指定した時間の後閉じられるチャネルを返します.

```clojure
(require '[clojure.core.async :refer [timeout <!!]])

(assert (= (<!! (timeout 1000)) nil))
```

実行すると1秒後に値が返ります.

## チャネルの選択

`alts!!`は複数のチャネルの内, 書き込みが行われたチャネルと値のペアを返します.

```clojure
(require '[clojure.core.async :refer [chan >!! alts!!]])

(let [c1 (chan)
      c2 (chan)]
  (future (>!! c1 :foo)
          (>!! c2 :bar))
  (assert (= (alts!! [c1 c2]) [:foo c1]))
  (assert (= (alts!! [c1 c2]) [:bar c2])))
```

まず`c1`に`:foo`を書き込まれます.

ここで`c1`に書き込まれたので`alts!!`は`:foo`と`c1`を返します.

次に値が読み出されたことで`c2`に`:bar`が書き込まれます.

ここで`c2`に書き込まれたので`alts!!`は`:bar`と`c2`を返します.

`alt!!`は複数のチャネルの操作から一つ選択します.

```clojure
(require '[clojure.core.async :refer [chan >!! alt!!]])

(let [c1 (chan)
      c2 (chan)]
  (future (>!! c1 :foo)
          (>!! c2 :bar))
  (assert (= (alt!! c1 ([v c] [v c])
                    c2 ([v c] [v c]))
             [:foo c1]))
  (assert (= (alt!! c1 ([v c] [v c])
                    c2 ([v c] [v c]))
             [:bar c2])))
```

この例は`alts!!`と同じ動作をします.

これらの関数は`timeout`と組み合わせることでタイムアウト処理を簡単に記述することができます.

```clojure
(require '[clojure.core.async :refer [timeout alt!!]])

(assert (= (alt!! (timeout 10000) ([] :foo)
                  (timeout 100) ([] :bar))
           :bar))
```

10000ms後に閉じられるチャネルと100ms後に閉じられるチャネルでは, 後者の方が早くに閉じられます.

## go

`go`はcore.asyncが提供する非同期実行の仕組みです.

`go`の内側では`>!!`と`<!!`の代わりに, `>!`と`<!`を用います.

```clojure
(require '[clojure.core.async :refer [chan <! >! go]])

(let [c (chan)]
  (go (loop [n 0]
        (>! c n)
        (recur (inc n))))
  (go (assert (= (<! c) 0))
      (assert (= (<! c) 1))
      (assert (= (<! c) 2))))
```

チャネルから値を読み出す毎に値がインクリメントされていることが確認できます.

`go`は`<!`や`>!`の呼び出しを見つけ出し, SSA形式のステートマシンを生成します.

継続はcore.asyncのスレッドプールから生成されるスレッドで処理されます.

スレッドプールのサイズは`プロセッサ数 * 2 + 42`です.

# algo.generic

Clojure標準の関数を総称的な関数として提供するライブラリです.

## Generic functions

総称的な関数を見ていきましょう.

次は四則演算の例です.

```clojure
(refer-clojure :exclude [+ - * /])
(require '[clojure.algo.generic.arithmetic :refer (+ - * /)])

(assert (= (+ 1 2) 3))
(assert (= (- 1 2) -1))
(assert (= (* 1 2) 2))
(assert (= (/ 1 2) 1/2))
```

`java.lang.Number`に対してclojure.coreの関数と変わりない動作をします.

algo.generic.arithmeticの`zero-type`と`one-type`について見てみましょう.

```clojure
(refer-clojure :exclude [+ - * /])
(require '[clojure.algo.generic.arithmetic :refer (+ - * / zero one)])

(assert (= (+ 2 zero) 2))
(assert (= (- 3 zero) 3))
(assert (= (* 5 one) 5))
(assert (= (/ 7 one) 7))
(assert (= (+) zero))
(assert (= (*) one))
```

`zero`は加算における単位元, `one`は乗算における単位元として働いています.

これらの関数はMultimethodとして提供されておりユーザーが拡張することが可能です.

複素数とそれに対する演算を定義してみましょう.

```clojure
(ns complex
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.algo.generic.arithmetic :refer (+ - * / zero one)]))

(defrecord Complex [re im])

(defmethod + [Complex Number] [c n] (+ c (Complex. n 0)))
(defmethod + [Number Complex] [n c] (+ (Complex. n 0) c))
(defmethod + [Complex Complex] [c c']
  (Complex. (+ (.re c) (.re c')) (+ (.im c) (.im c'))))

(assert (= (+ (Complex. 1 1) (Complex. 1 1)) (Complex. 2 2)))
(assert (= (+ (Complex. 1 1) 1) (Complex. 2 1)))
(assert (= (+ (Complex. 1 1) zero) (Complex. 1 1)))

(defmethod - [Complex Number] [c n] (- c (Complex. n 0)))
(defmethod - [Number Complex] [n c] (- (Complex. n 0) c))
(defmethod - [Complex Complex] [c c']
  (Complex. (- (.re c) (.re c')) (- (.im c) (.im c'))))

(assert (= (- (Complex. 1 1) (Complex. 1 1)) (Complex. 0 0)))
(assert (= (- (Complex. 1 1) 1) (Complex. 0 1)))
(assert (= (- (Complex. 1 1) zero) (Complex. 1 1)))

(defmethod * [Complex Number] [c n] (* c (Complex. n 0)))
(defmethod * [Number Complex] [n c] (* (Complex. n 0) c))
(defmethod * [Complex Complex] [c c']
  (Complex. (- (* (.re c) (.re c')) (* (.im c) (.im c')))
            (+ (* (.re c) (.im c')) (* (.im c) (.re c')))))

(assert (= (* (Complex. 1 1) (Complex. 1 1)) (Complex. 0 2)))
(assert (= (* (Complex. 1 1) 0) (Complex. 0 0)))
(assert (= (* (Complex. 1 1) one) (Complex. 1 1)))

(defmethod / [Complex Number] [c n] (/ c (Complex. n 0)))
(defmethod / [Number Complex] [n c] (/ (Complex. n 0) c))
(defmethod / [Complex Complex] [c c']
  (let [d (+ (* (.re c') (.re c')) (* (.im c') (.im c')))]
    (Complex. (/ (+ (* (.re c) (.re c')) (* (.im c) (.im c'))) d)
              (/ (- (* (.im c) (.re c')) (* (.re c) (.im c'))) d))))

(assert (= (/ (Complex. 1 1) (Complex. 1 1)) (Complex. 1 0)))
(assert (= (/ (Complex. 1 1) (Complex. 0 1)) (Complex. 1 -1)))
(assert (= (/ (Complex. 1 1) one) (Complex. 1 1)))
```

`+`, `-`, `*`, `/`が複素数に対して使えるようになりました.

総称的な関数を使った関数もまた総称的なものになります.

```clojure
(refer-clojure :exclude [*])
(require '[clojure.algo.generic.arithmetic :refer (*)])
(require '[complex :refer (->Complex)])

(defn square [x] (* x x))

(assert (= (square 3) 9))
(assert (= (square (->Complex 1 -1)) (->Complex 0 -2)))
```

四則演算だけでなく, モジュール毎に総称的な関数が定義されています.

* algo.generic.arithmetic
* algo.generic.collection
* algo.generic.comparison
* algo.generic.math-functions
* algo.generic.functor

# [core.logic](core_logic.html)

Clojureで論理プログラミングをするためのライブラリです.

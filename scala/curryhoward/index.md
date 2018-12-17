---
layout: article
title: Scalaの合併型と全称型
---

# Scalaの合併型と全称型

カリー＝ハワード同型対応から交差型と存在型を用いて合併型と全称型を導きます。

## Scalaの型と論理

Scalaが標準で備える型と論理の対応は次のようになります。

<table class="table">
  <thead>
    <tr><th>型</th><th>論理</th></tr>
  </thead>
  <tbody>
    <tr><td>Any</td><td>真</td></tr>
    <tr><td>Nothing</td><td>偽</td></tr>
    <tr><td>A with B</td><td>A かつ B</td></tr>
    <tr><td>A => B または A <:< B</td><td>A ならば B</td></tr>
    <tr><td>P[A] forSome { type A }</td><td>P[A] である A が存在する</td></tr>
  </tbody>
</table>

`Any`は全ての型のスーパータイプ、`Nothing`は全ての型のサブタイプです。

`A with B`は`A`と`B`の交差型を作ります。

`A <:< B`は`A`が`B`のサブタイプであることを表します。

`forSome`は存在型を構成します。

## 否定型

この対応から論理における否定は次のようになります。

```scala
type Not[A] = A => Nothing
```

`Nothing`はボトム型なので`Nothing`以外の型では包含関係を満たせないことがわかります。

また、Scalaの型システムにおいて`Not[Not[A]] =:= A`が成り立たないことに注意しましょう。

## 合併型

この否定型と交差型を使って合併型を作ることができます。

```scala
type Or[A, B] = Not[Not[A] with Not[B]]
```

この定義はド・モルガンの法則から導かれます。

合併型は次のように使うことができます。

```scala
def double[A](a: A)(implicit ev: Not[Not[A]] <:< Or[Int, String]): String =
  a match {
    case i: Int => (i + i).toString
    case s: String => s + s
  }

assert(double(2) == "4")
assert(double("2") == "22")
```

`Or`が二重否定を含むため`Not[Not[A]]`とすることで型を合わせています。

この`double`関数は`Int`型と`String`型以外の値を受け付けません。

また、`Either`に比べ値をラップする必要がなく効率が良いです。

### 構造的部分型

構造的な型に対しても合併型は直感的な包含関係を満たします。

構造的な型に対する交差型は各フィールドの和集合を持つ型をスーパータイプに持ちます。

```scala
type Foo = { def foo: String; def baz: Int }
type Bar = { def bar: String; def baz: Int }
implicitly[Foo with Bar <:< { def foo: String; def bar: String; def baz: Int }]
```

構造的な型に対する合併型は各フィールドの積集合を持つ型をスーパータイプに持ちます。

```scala
type Foo = { def foo: String; def baz: Int }
type Bar = { def bar: String; def baz: Int }
implicitly[Or[Foo, Bar] <:< Not[Not[{ def baz: Int }]]]
```

Scalaでは構造的部分型があまり使われませんが、例えばJSONの型付けなどに利用できるかもしれません。

## 全称型

Scalaにおける全称型は次のように定義できます。

```scala
trait Forall[P[_]] {
  def apply[A]: P[A]
}
```

これを用いると任意の型を扱う値を表現できます。

```scala
type Nat[F[_], G[_]] = Forall[({ type H[A] = F[A] => G[A] })#H]

def mapping[F[_], G[_], A, B](f: Nat[F, G])(pair: (F[A], F[B])): (G[A], G[B]) = (f[A](pair._1), f[B](pair._2))

def opt2list: Nat[Option, List] =
  new Nat[Option, List] {
    def apply[A]: Option[A] => List[A] = _.toList
  }

assert(mapping(opt2list)((Some("hoge"), None)) == (List("hoge"), Nil))

def list2opt: Nat[List, Option] =
  new Nat[List, Option] {
    def apply[A]: List[A] => Option[A] = _.headOption
  }

assert(mapping(list2opt)((List(0, 1, 2), Nil)) == (Some(0), None))
```

`Nat[F, G]`は任意の型`A`に対して`F[A]`を`G[A]`に対応させる型です。

`mapping`は`(F[A], F[B])`を`Nat[F, G]`を使って`(G[A], G[B])`に変換します。

この全称型もまた否定型と存在型により表現が可能です。

`全てのAはPである`ということは`PでないAは存在しない`と言い換えることができます。

```scala
type Forall[P[_]] = Not[Not[P[A]] forSome { type A }]
```

この全称型も合併型と同様に二重否定を含みます。

継続渡し形式(CPS)により二重否定を導入できます。

```scala
def opt2list: Nat[Option, List] = k => k(_.toList)

def list2opt: Nat[List, Option] = k => k(_.headOption)
```

利用する際には二重否定を除去する必要があります。

最も簡単な定義は`return`を使ったものです。

```scala
def callCC[A](f: Not[Not[A]]): A = f(a => return a)
```

これで最初に定義した全称型と同じように使えます。

```scala
def mapping[F[_], G[_], A, B](f: Nat[F, G])(pair: (F[A], F[B])): (G[A], G[B]) =
  (callCC[F[A] => G[A]](f)(pair._1), callCC[F[B] => G[B]](f)(pair._2))

assert(mapping(opt2list)((Some("hoge"), None)) == (List("hoge"), Nil))

assert(mapping(list2opt)((List(0, 1, 2), Nil)) == (Some(0), None))
```

この定義は関数リテラルが使えるため`trait`で定義した場合よりも簡単に全称型を作れます。

## 参考

* [Unboxed union types in Scala via the Curry-Howard isomorphism](http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/)
* [scalaz.Forall を読む](http://d.hatena.ne.jp/leque/20111226/p1)

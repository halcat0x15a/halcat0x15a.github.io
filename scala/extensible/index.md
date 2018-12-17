---
layout: article
title: Extensible Effects in Scala
---

# Extensible Effects in Scala

[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf) で紹介される Eff モナドを Scala で解説します。

### Contents

1. [Free Monad](#free-monad)
2. [Freer Monad](#freer-monad)
3. [Efficient Freer](#efficient-freer)
4. [Eff Monad (Extensible Freer)](#eff-monad)

## Free Monad

Free は Functor を与えることでモナドになるデータ型です。

Functor の定義から見ていきましょう。

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

計算コンテナ `F` に対して `map` という関数が定義されます。

`map` は `F[A]` の計算値 `A` に関数 `A => B` を適用し `F[B]` を得ます。

この Functor を使って Free は次のように定義されます。

```scala
sealed trait Free[F[_], A] {

  def map[B](f: A => B)(implicit F: Functor[F]): Free[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Free[F, B])(implicit F: Functor[F]): Free[F, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(ff) => Impure(F.map(ff)(_.flatMap(f)))
    }

}

case class Pure[F[_], A](a: A) extends Free[F, A]

case class Impure[F[_], A](ff: F[Free[F, A]]) extends Free[F, A]
```

Free は Functor `F` と計算値 `A` を型パラメータにとります。

`flatMap` は Pure ならば関数に値を適用し、Impure ならば Functor を使って `F` の計算値に `f` を適用します。

`F` のパラメータの扱いによって様々な再帰的なデータ構造を表現することができます。

Free を使って二分木を表現してみましょう。

```scala
type Pair[A] = (A, A)

implicit val PairFunctor: Functor[Pair] =
  new Functor[Pair] {
    def map[A, B](fa: Pair[A])(f: A => B): Tree[B] =
      fa match {
        case (x, y) => (f(x), f(y))
      }
  }
```

Pair は要素をただ二つだけ持つコンテナです。

Pair の Functor はそれぞれの要素に関数を適用し、新たに Pair を構築します。

Pair に Free を適用することで Tree を構成できます。

```scala
type Tree[A] = Free[Pair, A]

def leaf[A](a: A): Tree[A] = Pure(a)

def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]])
```

`leaf` は Pure で表現され、node は Tree を再帰的に持つ Impure で表現されます。

Pair が Functor のインスタンスを持つため Tree はモナドになります。

```scala
val r = for {
  x <- node(leaf(0), node(leaf(1), leaf(2)))
  y <- node(leaf(x), leaf(x))
} yield y + 1

assert(r == node(node(leaf(1), leaf(1)), node(node(leaf(2), leaf(2)), node(leaf(3), leaf(3)))))
```

これは `leaf(x)` を `node(leaf(x + 1), leaf(x + 1))` で置換するような計算です。

このように、Free は型パラメータ `F` に Functor を与えることで様々なモナドを表現することができます。

例えば、我々が普段使っている List や Option、Either なども表現可能です。

## Freer Monad

Free モナドから Functor の制約をなくしたものが Freer モナドです。

これには Coyoneda と呼ばれる構造を Free に加えます。

```scala
trait Coyoneda[F[_], A, B] { self =>
  val fa: F[A]
  val k: A => B
  def map[C](f: B => C): Coyoneda[F, A, C] =
    new Coyoneda[F, A, C] {
      val fa: F[A] = self.fa
      val k: A => C = self.k andThen f
    }
}
```

Coyoneda は任意の `F[_]` と始域 `A` と終域 `B` を型パラメータにとります。

Coyonedaは `map` を持つため Functor のインスタンスになります。

つまり、Free に Coyoneda の構造を加えることで、任意の `F` からモナドを構成できるようになります。

```scala
sealed trait Freer[F[_], A] {

  def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(fa, k) => Impure(fa, (a: Any) => k(a).flatMap(f))
    }

}

case class Pure[F[_], A](a: A) extends Freer[F, A]

case class Impure[F[_], A, B](fa: F[A], k: A => Freer[F, B]) extends Freer[F, B]
```

Free では Impure が `F` を Free に適用することで再帰的な構造を表していたのに対し、Freer では Impure が `F[A]` とその継続の計算として `A => Freer[F, B]` をとることで再帰的な構造を表しています。

`flatMap` は Impure の場合に Freer モナドの元で関数の合成を行っており、これは Kleisli composition と呼ばれます。

Free と同じように作用のある計算を記述するには、次のような関数があると便利でしょう。

```scala
object Freer {
  def apply[F[_], A](ff: F[Freer[F, A]]): Freer[F, A] =
    Impure(ff, (x: Freer[F, A]) => x)
}
```

Freer を使って Maybe (Option) を表現してみましょう。

```scala
type ConstUnit[A] = Unit

type Maybe[A] = Freer[ConstUnit, A]

def some[A](a: A): Maybe[A] = Pure(a)

def none[A]: Maybe[A] = Freer((): ConstUnit[Maybe[A]])
```

Maybe は値を含まないかもしれない計算を表現します。

値が存在する場合は `some` で、存在しない場合は `none` で Maybe を構築します。

Maybe を使った簡単な例を示します。

```scala
def safeDiv(n: Int, d: Int): Maybe[Int] = if (d == 0) none else some(n / d)

val r = for {
  n <- safeDiv(4, 2)
  m <- safeDiv(n, 0)
} yield m
```

Free と違って Functor のインスタンスを定義することなくモナドが得られました。

しかし、Freer は関数をデータ構造に持っているので単純な比較ができなくなりました。

Maybe を実行する関数を定義して `r` の計算結果を確認します。

```scala
def maybe[A](m: Maybe[A])(default: A): A = m match {
  case Pure(a) => a
  case Impure((), _) => default
}

assert(maybe(r)(42) == 42)
```

このように、Freer は Free より簡単にモナドを得ることができます。

## Efficient Freer

Freer の flatMap の実装には問題があります。

```scala
def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
  this match {
    case Pure(a) => f(a)
    case Impure(fa, k) => Impure(fa, (a: Any) => k(a).flatMap(f))
  }
```

Pure に到達するまで再帰的に `flatMap` を呼び出しています。

これは次のようなケースで時間計算量が `O(n^2)` になります。

`x.flatMap(f_1).flatMap(f_2) ... .flatMap(f_n)`

この問題を改善するために新しいデータ構造を加えます。

### Fast type-aligned queue

`Arrows[F, A, B]` は関数 `A => Freer[F, B]` を表現する二分木です。

```scala
sealed trait Arrows[F[_], A, B] {

  def :+[C](f: B => Freer[F, C]): Arrows[F, A, C] = Node(this, Leaf(f))

  def ++[C](q: Arrows[F, B, C]): Arrows[F, A, C] = Node(this, q)

}

case class Leaf[F[_], A, B](f: A => Freer[F, B]) extends Arrows[F, A, B]

case class Node[F[_], A, B, C](left: Arrows[F, A, B], right: Arrows[F, B, C]) extends Arrows[F, A, C]
```

要素の追加と連結は関数の合成を意味し、定数時間で実行されます。

Arrows を使うことで Freer は次のように定義されます。

```scala
sealed trait Freer[F[_], A] {

  def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(fa, k) => Impure(fa, k :+ f)
    }

}

case class Pure[F[_], A](a: A) extends Freer[F, A]

case class Impure[F[_], A, B](fa: F[A], k: Arrows[F, A, B]) extends Freer[F, B]

object Freer {

  def apply[F[_], A](fa: F[Freer[F, A]]): Freer[F, A] = Impure(fa, Leaf((a: Freer[F, A]) => a))

}
```

これで `n` 回の `flatMap` による合成が `O(n)` で実行できます。

### Left-edge deconstruction

Arrows の関数適用を記述するために新たなデータ構造を定義します。

View は Arrows から右結合な構造を構築します。

```scala
sealed trait Arrows[F[_], A, B] {

  def view: View[F, A, B] =
    this match {
      case Leaf(f) => One(f)
      case Node(l, r) =>
        @scala.annotation.tailrec
        def go(x: Arrows[F, A, Any], y: Arrows[F, Any, B]): View[F, A, B] =
          x match {
            case Leaf(f) => Cons(f, y)
            case Node(l, r) => go(l, Node(r, y))
          }
        go(l, r)
    }

}

sealed trait View[F[_], A, B]

case class One[F[_], A, B](f: A => Freer[F, B]) extends View[F, A, B]

case class Cons[F[_], A, B, C](f: A => Freer[F, B], k: Arrows[F, B, C]) extends View[F, A, C]
```

`Leaf` は `One` に対応し、`Node` は左の要素を分解し右の要素に連結させることで `Cons` に対応します。

`view` 関数はならし定数時間で実行されます。

このデータ構造を使うことで関数適用は次のように定義できます。

```scala
sealed trait Arrows[F[_], A, B] {

  def apply(a: A): Freer[F, B] = {
    @scala.annotation.tailrec
    def go(f: Arrows[F, Any, B], a: Any): Freer[F, B] =
      f.view match {
        case One(f) => f(a)
        case Cons(f, r) =>
          f(a) match {
            case Pure(v) => go(r, v)
            case Impure(f, l) => Impure(f, l ++ r)
          }
      }
    go(this.asInstanceOf[Arrows[F, Any, B]], a)
  }

}
```

これで Freer で示した例を同様に記述することができます。

## Eff Monad

Maybe モナド以外のモナドを Freer を使って表現してみましょう。

Writer モナドは計算値以外に出力の値を持ちます。

```scala
```

ここまでで Tree モナドと Maybe モナドの Freer による表現を紹介しました。

これらのモナドを組み合わせて使うことは可能でしょうか。

### Open Union

型の和を Freer モナドに与えることでこの問題を解決します。

```scala
sealed trait Union

sealed trait Void extends Union

sealed trait :+:[F[_], U <: Union] extends Union

case class Inl[F[_], A, U <: Union](fa: F[A]) extends (F :+: U)

case class Inr[F[_], U <: Union](u: U) extends (F :+: U)
```

`:+:` が型の和を構成し、`Void` がその終端を表現します。

Union の値は次のように作ることができます。

```scala
type Tree[A] = (A, A)

type Maybe[A] = Unit

val tree1: Maybe :+: Tree :+: Void = Inr(Inl((0, 1): Tree[Int]))
```

Union の導入に伴い Tree や Maybe を構成する型 `F` 自体にそのモナドの名前を付けることにします。

この例からわかる通り、Inl と Inr を使って型を合わせる必要があります。

そこで、Union へ値を埋め込むための型クラスを導入します。

```scala
trait Member[F[_], U <: Union] {

  def inject[A](f: F[A]): U

}

object Member {

  implicit def left[F[_], U <: Union]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](f: F[A]): F :+: U = Inl(f)
    }

  implicit def right[F[_], G[_], U <: Union](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](f: F[A]): G :+: U = Inr(member.inject(f))
    }

}
```

`inject` はモナドを構成する `F` から Union `U` を得ます。

左側への埋め込みは `Member.left` が、右側への埋め込みは `Member.right` が行います。

Member のインスタンスを得るために次のような関数を定義しておくと便利でしょう。

```scala
object Member {

  def apply[F[_], U <: Union](implicit member: Member[F, U]): Member[F, U] = member

}
```

先の例で Member を利用すると次のようになります。

```scala
val tree2 = Member[Tree, Maybe :+: Tree :+: Void].inject((0, 1): Tree[Int])
```

型に合わせて `Inl` と `Inr` を書かずに済むようになりました。

Union を使った Freer を Eff と呼ぶことにします。

Eff は次のように定義されます。

```scala
sealed trait Eff[U <: Union, A] {

  def map[B](f: A => B): Eff[U, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Eff[U, B]): Eff[U, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(u, g) => Impure(u, g :+ f)
    }

}

case class Pure[U <: Union, A](a: A) extends Eff[U, A]

case class Impure[U <: Union, A, B](u: U, f: Arrows[U, A, B]) extends Eff[U, B]
```

`F[_]` を `U <: Union` で置き換えました。

Arrows と View にも同様の変更を加えます。

また、Member を使うことで作用のある計算は次のように構築できます。

```scala
object Eff {

  def apply[U <: Union, F[_], A](fa: F[Eff[U, A]])(implicit F: Member[F, U]): Eff[U, A] =
    Impure(F.inject(fa), Leaf((x: Eff[U, A]) => x))

}
```

これらを用いて Tree モナドは次のように定義されます。

```scala
def leaf[U <: Union, A](a: A)(implicit member: Member[Tree, U]): Eff[U, A] = Pure(a)

def node[U <: Union, A](x: Eff[U, A], y: Eff[U, A])(implicit member: Member[Tree, U]): Eff[U, A] = Eff((x, y): Tree[Eff[U, A]])

def fold[U <: Union, A, B](t: Eff[Tree :+: U, A])(f: A => B)(g: (B, B) => B): Eff[U, B] =
  t match {
    case Pure(a) => Pure(f(a))
    case Impure(u, h) =>
      def k(t: Tree[Any]): Eff[U, B] =
        t match {
          case (x, y) =>
            for {
              a <- fold(h(x))(f)(g)
              b <- fold(h(y))(f)(g)
            } yield g(a, b)
        }
      u match {
        case Inl(t) => k(t)
        case Inr(u) => Impure(u, Leaf(k))
      }
  }

def str[U <: Union, A, B](t: Eff[Tree :+: U, A]): Eff[U, String] = fold(t)(_.toString)((x, y) => s"($x, $y)")
```

`fold` の定義は少し複雑になりました。

`fold` は `Eff[Tree :+: U, A]` から関数 `A => B` と `(B, B) => B` を用いて `Eff[U, B]` を構築します。

Pure の場合はその値を関数に適用し Pure に包んで返します。

Impure の場合は継続 `k` が定義されます。

`k` は Tree のそれぞれの値に `h` と `fold` を適用し、その計算結果に `g` を適用することで畳み込みを行います。

Union が Inl の場合はその値を `k` に適用し、Inr の場合はその値と `k` から Impure を構築します。

ここで重要なのは再帰呼び出しによる継続の実行です。

ある程度パターン化されているので型を合わせることで自然に定義することが可能です。

Eff のパラメータ `U` は漸減し、最終的に Void になります。

`Eff[Void, A]` から値を取り出す関数は次のように定義されます。

```scala
object Eff {

  def run[A](eff: Eff[Void, A]): A =
    eff match {
      case Pure(a) => a
    }

}
```

`Eff[Void, A]` は Impure を値に持たないため、安全に実行されます。

Tree モナドは次のように使えます。

```scala
type U = Tree :+: Void

implicit val m = Member[Tree, U]

val r = for {
  x <- node(leaf(0), node(leaf(1), leaf(2)))
  y <- node(leaf(x), leaf(x))
} yield y + 1

assert(Eff.run(str(r)) == "((1, 1), ((2, 2), (3, 3)))")
```

Tree の例の実装は今までと同様です。

`Eff.run` により最終的な結果を取り出しています。

Maybe モナドも同様に定義してみましょう。

```scala
def nothing[U <: Union, A](implicit member: Member[Maybe, U]): Eff[U, A] = Eff((): Maybe[Eff[U, A]])

def just[U <: Union, A](a: A)(implicit member: Member[Maybe, U]): Eff[U, A] = Pure(a)

def maybe[U <: Union, A](m: Eff[Maybe :+: U, A])(default: A): Eff[U, A] =
  m match {
    case Pure(a) => Pure(a)
    case Impure(Inl(()), _) => Pure(default)
    case Impure(Inr(u), k) => Impure(u, Leaf((x: Any) => maybe(k(x))(default)))
  }
```

`maybe` は Impure が Inl を持つならばデフォルト値を返し、Inr を持つならばその Union `u` の継続で `maybe` を実行します。

Maybe モナドは次のように使えます。

```scala
type U = Maybe :+: Void

implicit val m = Member[Maybe, U]

val e1 = for {
  x <- just(2)
  y <- just(3)
} yield x + y

assert(Eff.run(maybe(e1)(-1)) == 5)

val e2 = for {
  x <- just(2)
  y <- nothing[U, Int]
} yield x + y

assert(Eff.run(maybe(e2)(-1)) == -1)
```

Eff でも Freer と同様の例が実行できました。

Eff ではこれらのモナドを一つのfor式で混合させることができます。

```scala
def e1[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Eff[U, Int] =
  for {
    x <- just(0)
    y <- just(1)
    z <- node(leaf(x), leaf(y))
  } yield z + 1

assert(Eff.run(maybe(str(e1[Tree :+: Maybe :+: Void]))("fail")) == "(1, 2)")
```

ここでは Tree モナドと Maybe モナドが混在しています。

実行する際には具体的な Union を渡す必要があります。

モナドの実行順序は自由に変えることができます。

```scala
def e2[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Eff[U, Int] =
  for {
    x <- just(0)
    y <- nothing[U, Int]
    z <- node(leaf(x), leaf(y))
  } yield z + 1

assert(Eff.run(maybe(str(e2[Tree :+: Maybe :+: Void]))("fail")) == "fail")

assert(Eff.run(str(maybe(e2[Maybe :+: Tree :+: Void])(-1))) == "-1")
```

Eff についてまとめると次のようになります。

* 様々なモナドを表現できる
* 複数の作用を混在させることが可能
* モナドの合成が高速

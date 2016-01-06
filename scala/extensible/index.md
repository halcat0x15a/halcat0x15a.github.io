---
layout: default
title: Extensible Effects in Scala
---

# Extensible Effects in Scala

[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)で紹介されるEffモナドをScalaを使って解説します。

## Free Monad

FreeモナドはFunctorを与えることでモナドになります。

Functorの定義から見ていきましょう。

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

計算コンテナ`F`に対して`map`という関数が定義されます。

`map`は`F[A]`の計算値`A`を関数`A => B`に適用し`F[B]`を得ます。

このFunctorを使ってFreeは次のように定義されます。

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

FreeはFunctor`F`と計算値`A`を型パラメータとります。

Pureは`A`の値を、Impureは`F`に`Free[F, A]`を適用した値を持ちます。

`flatMap`はPureならば値を関数に適用し、ImpureならばFunctorを使って`F`の値を`f`に適用します。

`F`のパラメータの扱いによって様々な再帰的なデータ構造を表現することができます。

Freeを使って二分木を表現してみましょう。

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

Pairは要素をただ二つだけ持つコンテナです。

PairのFunctorはそれぞれの要素を関数に適用し、Pairを構築します。

PairにFreeを適用することでTreeになります。

```scala
type Tree[A] = Free[Pair, A]

def leaf[A](a: A): Tree[A] = Pure(a)

def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]])
```

Treeはモナドです。

```scala
val r = for {
  x <- node(leaf(0), node(leaf(1), leaf(2)))
  y <- node(leaf(x), leaf(x))
} yield y + 1

assert(r == node(node(leaf(1), leaf(1)), node(node(leaf(2), leaf(2)), node(leaf(3), leaf(3)))))
```

これは`leaf(x)`を`node(leaf(x + 1), leaf(x + 1))`で置換するようなモナド計算です。

このように、Freeには様々なモナドを表現する力があります。

## Freer Monad

FreeモナドからFunctorの制約をなくしたものがFreerモナドになります。

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

Freeモナドとの違いとして、Impureは`F[A]`と`A => Freer[F, B]`の組みを持つ`Freer[F, B]`になりました。

この変更により、`flatMap`はImpureの場合にFreerモナドの元で関数の合成(Kleisli composition)を行います。

Freeモナドと同じように作用のある計算を記述するには、次のような関数があると便利でしょう。

```scala
object Freer {

  def apply[F[_], A](ff: F[Freer[F, A]]): Freer[F, A] =
    Impure(ff, (x: Freer[F, A]) => x)

}
```

Freerを使ってTreeを表現してみましょう。

```scala
type Pair[A] = (A, A)

type Tree[A] = Freer[Pair, A]

def leaf[A](a: A): Tree[A] = Pure(a)

def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Freer((x, y): Pair[Tree[A]])
```

先の例も同様に記述できます。

```scala
val r = for {
  x <- node(leaf(0), node(leaf(1), leaf(2)))
  y <- node(leaf(x), leaf(x))
} yield y + 1
```

Treeに対する畳み込み関数を定義し、文字列に変換する関数を定義してみましょう。

```scala
def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
  t match {
    case Pure(a) => f(a)
    case Impure((x, y), k) => g(fold(k(x))(f)(g), fold(k(y))(f)(g))
  }

def str[A](t: Tree[A]): String = fold(t)(_.toString)((x, y) => s"($x, $y)")

assert(str(r) == "((1, 1), ((2, 2), (3, 3)))")
```

このように、FreerはFreeより簡単にモナドを得ることができます。

## Efficient Freer

FreerのflatMapの実装には問題があります。

```scala
  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(fa, k) => Impure(fa, (a: Any) => k(a).flatMap(f))
    }
```

Pureに到達するまで再帰的に`flatMap`を呼び出しています。

これは次のようなケースで時間計算量がO(n^2)になります。

`x.flatMap(f_1).flatMap(f_2) ... .flatMap(f_n)`

この問題を改善するために新しいデータ構造を加えます。

### Fast type-aligned queue

`Arrows[F, A, B]`は関数`A => Freer[F, B]`を表現する二分木です。

```scala
sealed trait Arrows[F[_], A, B] {

  def :+[C](f: B => Freer[F, C]): Arrows[F, A, C] = Node(this, Leaf(f))

  def ++[C](q: Arrows[F, B, C]): Arrows[F, A, C] = Node(this, q)

}

case class Leaf[F[_], A, B](f: A => Freer[F, B]) extends Arrows[F, A, B]

case class Node[F[_], A, B, C](left: Arrows[F, A, B], right: Arrows[F, B, C]) extends Arrows[F, A, C]
```

要素の追加と連結は関数の合成を意味し、定数時間で実行されます。

Arrowsを使うことでFreerは次のように定義されます。

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

これでn回の`flatMap`による合成がO(n)で実行できます。

### Left-edge deconstruction

Arrowsの関数適用を記述するために新たなデータ構造を定義します。

ViewはArrowsから右結合な構造を構築します。

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

`Leaf`は`One`に対応し、`Node`は左の要素を分解し右の要素に連結させることで`Cons`に対応します。

`view`関数はならし定数時間で実行されます。

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

これでFreerで示した例を記述することができます。

## Extensible Freer

Treeモナド以外のモナドも定義してみましょう。

次は失敗するかもしれない計算をFreerで表したものです。

```scala
type Const[A] = Unit

type Maybe[A] = Freer[Const, A]

def nothing[A]: Maybe[A] = Impure((): Const[Maybe[A]], (x: Maybe[A]) => x)

def just[A](a: A): Maybe[A] = Pure(a)

def run[A](m: Maybe[A])(default: A): A =
  m match {
    case Pure(a) => a
    case Impure((), _) => default
  }
```

`just`が成功を表し、`nothing`が失敗を表します。

これは次のように利用できます。

```scala
val e1 = for {
  x <- just(2)
  y <- just(3)
} yield x + y

assert(maybe(e1)(-1) == 5)

val e2 = for {
  x <- just(2)
  y <- nothing[Int]
} yield x + y

assert(maybe(e2)(-1) == -1)
```

このMaybeモナドとTreeモナドを組み合わせて使うことはできるでしょうか。

### Open Union

型の和をFreerモナドに与えることでこの問題を解決します。

```scala
sealed trait Union

sealed trait Void extends Union

sealed trait :+:[F[_], U <: Union] extends Union

case class Inl[F[_], A, U <: Union](fa: F[A]) extends (F :+: U)

case class Inr[F[_], U <: Union](u: U) extends (F :+: U)
```

`:+:`が型の和を構成し、`Void`がその終端を表現します。

Unionの値は次のように作ることができます。

```scala
type Tree[A] = (A, A)

type Maybe[A] = Unit

val tree1: Maybe :+: Tree :+: Void = Inr(Inl((0, 1): Tree[Int]))
```

Unionの導入に伴いTreeやMaybeを構成する型`F`自体にそのモナドの名前を付けることにします。

この例からわかる通り、InlとInrを使って型を合わせる必要があります。

そこで、Unionへ値を埋め込むための型クラスを導入します。

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

`inject`はモナドを構成する`F`からUnion`U`を得ます。

左側への埋め込みは`Member.left`が、右側への埋め込みは`Member.right`が行います。

Memberのインスタンスを得るために次のような関数を定義しておくと便利でしょう。

```scala
object Member {

  def apply[F[_], U <: Union](implicit member: Member[F, U]): Member[F, U] = member

}
```

先の例でMemberを利用すると次のようになります。

```scala
val tree2 = Member[Tree, Maybe :+: Tree :+: Void].inject((0, 1): Tree[Int])
```

型に合わせて`Inl`と`Inr`を書かずに済むようになりました。

Unionを使ったFreerをEffと呼ぶことにします。

Effは次のように定義されます。

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

`F[_]`を`U <: Union`で置き換えました。

ArrowsとViewにも同様の変更を加えます。

また、Memberを使うことで作用のある計算は次のように構築できます。

```scala
object Eff {

  def apply[U <: Union, F[_], A](fa: F[Eff[U, A]])(implicit F: Member[F, U]): Eff[U, A] =
    Impure(F.inject(fa), Leaf((x: Eff[U, A]) => x))

}
```

これらを用いてTreeモナドは次のように定義されます。

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

`fold`の定義は少し複雑になりました。

`fold`は`Eff[Tree :+: U, A]`から関数`A => B`と`(B, B) => B`を用いて`Eff[U, B]`を構築します。

Pureの場合はその値を関数に適用しPureに包んで返します。

Impureの場合は継続`k`が定義されます。

`k`はTreeのそれぞれの値に`h`を適用し、その結果に`g`を適用します。

UnionがInlの場合はその値を`k`に適用し、Inrの場合はその値と`k`からImpureを構築します。

Effのパラメータ`U`は漸減し、最終的にVoidになります。

`Eff[Void, A]`から値を取り出す関数は次のように定義されます。

```scala
object Eff {

  def run[A](eff: Eff[Void, A]): A =
    eff match {
      case Pure(a) => a
    }

}
```

`Eff[Void, A]`はImpureを値に持たないため、安全に実行されます。

Treeモナドは次のように使えます。

```scala
type U = Tree :+: Void

implicit val m = Member[Tree, U]

val r = for {
  x <- node(leaf(0), node(leaf(1), leaf(2)))
  y <- node(leaf(x), leaf(x))
} yield y + 1

assert(Eff.run(str(r)) == "((1, 1), ((2, 2), (3, 3)))")
```

例`r`の実装は今までと同様です。

`Eff.run`により最終的な結果を取り出しています。

Maybeモナドも同様に定義してみましょう。

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

`maybe`はImpureがInlを持つならばデフォルト値を返し、Inrを持つならばそのUnion`u`の継続で`maybe`を実行します。

Maybeモナドは次のように使えます。

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

EffでもFreerと同様の例が実行できました。

Effではこれらのモナドを一つのfor式で混合させることができます。

```scala
def e1[U <: Union](implicit t: Member[Tree, U], m: Member[Maybe, U]): Eff[U, Int] =
  for {
    x <- just(0)
    y <- just(1)
    z <- node(leaf(x), leaf(y))
  } yield z + 1

assert(Eff.run(maybe(str(e1[Tree :+: Maybe :+: Void]))("fail")) == "(1, 2)")
```

ここではTreeモナドとMaybeモナドが混在しています。

実行する際には具体的なUnionを渡す必要があります。

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

Effについてまとめると次のようになります。

* 様々なモナドを表現できる
* 複数の作用を混在させることが可能
* モナドの合成が高速

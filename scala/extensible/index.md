---
layout: default
title: Extensible Effects in Scala
---

# Extensible Effects in Scala

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

PairにFreeを適用することで`Tree`となります。

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
      case Impure(freer, k) => Impure(freer, (a: Any) => k(a).flatMap(f))
    }

}

case class Pure[F[_], A](a: A) extends Freer[F, A]

case class Impure[F[_], A, B](freer: Freer[F, A], k: A => Freer[F, B]) extends Freer[F, B]
```

Freeとの違いとして、Impureが`A`を値に持つFreerと`A`からFreerへの関数の組をとるようになりました。

この変更により、`flatMap`はImpureの場合にFreerモナドの元で関数の合成(Kleisli composition)を行います。

Freerを使ってTreeを表現してみましょう。

```scala
type Pair[A] = (A, A)

type Tree[A] = Freer[Pair, A]

def leaf[A](a: A): Tree[A] = Pure(a)

def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]], (x: Tree[A]) => x)
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

### Fast type-aligned queue

## Extensible Freer

### Open Union

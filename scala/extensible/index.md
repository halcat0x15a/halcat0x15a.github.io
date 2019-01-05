---
layout: article
title: Extensible Effects in Scala
---

# Extensible Effects in Scala

[Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf) で紹介される Eff モナドを Scala を使って解説します。

### Contents

1. [Free Monad](#free-monad)
2. [Freer Monad](#freer-monad)
3. [Efficient Freer](#efficient-freer)
4. [Eff Monad (Extensible Freer)](#eff-monad)

## Free Monad

Free はパラメータに Functor のインスタンスを与えることでモナドになるデータ型です。

Functor の定義から見ていきましょう。

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

計算コンテナ `F` に対して `map` という関数が定義されます。

`map` は `F[A]` の計算値 `A` に関数 `A => B` を適用し `F[B]` を得ます。

Scala では Option や List は Functor のインスタンスになります。

```scala
object Functor {
  implicit object OptionFunctor extends Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit object ListFunctor extends Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}
```

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

Free は Functor `F` と計算値 `A` を型パラメータにとり、2つのコンストラクタ Pure と Impure をもちます。

Pure は純粋な計算を表し、計算値 `A` をもちます。

Impure は副作用付きの計算を表し、Free に Functor `F` を再帰的に適用しています。

`flatMap` は Pure ならば関数に値を適用し、Impure ならば Functor を使って `F` の計算値に `f` を適用します。

パラメータ `F` によって様々な再帰的なデータ構造を表現することができます。

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

Pair は要素をただ2つだけもつコンテナです。

Pair の Functor はそれぞれの要素に関数を適用し、新たに Pair を構築します。

Pair に Free を適用することで Tree を構成できます。

```scala
type Tree[A] = Free[Pair, A]

def leaf[A](a: A): Tree[A] = Pure(a)

def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]])
```

`leaf` は Pure で表現され、`node` は Tree を再帰的にもつ Impure で表現されます。

Pair が Functor のインスタンスであるため Tree はモナドになります。

```scala
val r = for {
  x <- node(leaf(0), node(leaf(1), leaf(2)))
  y <- node(leaf(x), leaf(x))
} yield y + 1

assert(r == node(node(leaf(1), leaf(1)), node(node(leaf(2), leaf(2)), node(leaf(3), leaf(3)))))
```

これは `leaf(x)` を `node(leaf(x), leaf(x))` で置換し、その後 `leaf(y)` を `leaf(y + 1)` で置換するような計算です。

このように、Free は型パラメータ `F` に Functor を与えることで様々なモナドを表現することができます。

例えば、我々が普段使っている List や Option、Either なども表現可能です。

## Freer Monad

Free モナドから Functor の制約をなくしたものが Freer モナドです。

これには Coyoneda と呼ばれる構造を Free に加えます。

```scala
case class Coyoneda[F[_], A, B](fa: F[A], k: A => B) {
  def map[C](f: B => C): Coyoneda[F, A, C] =
    Coyoneda(fa, k andThen f)
}
```

Coyoneda は任意の `F[_]` と始域 `A` と終域 `B` を型パラメータにとります。

Coyoneda は `map` をもつため Functor のインスタンスになります。

つまり、Free に Coyoneda の構造を加えることで、任意の `F` からモナドを構成できるようになります。

これを Freer と呼び、以下のように定義されます。

```scala
sealed trait Freer[F[_], A] {
  def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(fa, k) => Impure(fa, (a: Any) => k(a) flatMap f)
    }
}

case class Pure[F[_], A](a: A) extends Freer[F, A]

case class Impure[F[_], A, B](fa: F[A], k: A => Freer[F, B]) extends Freer[F, B]
```

Coyoneda を加えたことで Impure が始域の計算 `F[A]` とその継続の計算として `A => Freer[F, B]` をもつようになりました。

`flatMap` は Functor の制約がなくなり、Impure の場合に Freer モナドの元で関数の合成を行っています。これは Kleisli composition と呼ばれます。

Free と同じように作用のある計算を記述するには、次のような関数があると便利です。

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

Free と違って Functor のインスタンスを定義することなくモナドを得ることができました。

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

Freer の flatMap の実装には計算量に関する問題があります。

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

これは左結合のリスト連結と似たようなパフォーマンスになります。

`((xs ++ ys) ++ zs)`

`++` は左辺のリストの長さだけ走査が行われるので、左結合だと `++` の呼び出しのたびに連結したリストを再び走査することになります。

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
        def go[T](x: Arrows[F, A, T], y: Arrows[F, T, B]): View[F, A, B] =
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

`view` 関数は平均定数時間で実行されます。

このデータ構造を使うことで Arrows の関数適用は次のように定義できます。

```scala
sealed trait Arrows[F[_], A, B] {
  def apply(a: A): Freer[F, B] = {
    @scala.annotation.tailrec
    def go[A](f: Arrows[F, A, B], a: A): Freer[F, B] =
      f.view match {
        case One(f) => f(a)
        case Cons(f, r) =>
          f(a) match {
            case Pure(v) => go(r, v)
            case Impure(f, l) => Impure(f, l ++ r)
          }
      }
    go(this, a)
  }
}
```

これで効率的な Freer モナドは完成です。

## Eff Monad

ここまでで Tree モナドと Maybe モナドを Free と Freer を使って表現しました。

Eff はこれらのモナドを組み合わせて使うことを可能にします。

### Open Union

Freer モナドに計算構造 `F` を与えることで様々なモナドを表現することができました。

この `F` に複数の構造をもたせるため、型の和 Union を導入します。

```scala
sealed trait Union[F[_], G[_], A]

case class Inl[F[_], G[_], A](value: F[A]) extends Union[F, G, A]

case class Inr[F[_], G[_], A](value: G[A]) extends Union[F, G, A]

sealed trait Void[A]
```

Union は高階型パラメータ `F[_]`, `G[_]` とそれらに適用される型パラメータ `A` をとります。

`Inl` は Union の型パラメータの左側の値 `F[A]` を、`Inr` は右側の値 `G[A]` を値にもちます。

`Void` は Union で構成される型の和の終端を表します。

このまま複数の型を Union で繋げて型の和を作ることもできますが、とても冗長な記述になります。

そこで以下のようなシンタックスシュガーを用意します。

```scala
type :+:[F[_], G[_]] = { type R[A] = Union[F, G, A] }
```

`:+:` は `F[_]` と `G[_]` をとりタイプメンバ `type R[A] = Union[F, G, A]` をもつ構造型を返します。

これにより、中置記法を用いて Union の値を次のように作ることができます。

```scala
val u1: (List :+: (Option :+: Void)#R)#R[Int] = Inr(Inl(Some(0)))
```

`u1` は List または Option を Int に適用した値をもちます。

Union の値を作るには `Inl` と `Inr` を使って型を合わせる必要があります。

この型合わせを自動化するために型クラスを導入します。

```scala
trait Member[F[_], G[_]] {
  def inject[A](f: F[A]): G[A]
}
```

Member は型 `F` が Union のサブタイプ `G` に含まれる制約を表現します。

`inject` は値 `F[A]` を Union の値 `G[A]` に埋め込みます。

型クラス Member は2つのインスタンスをもちます。

```scala
object Member {
  implicit def left[F[_], G[_]]: Member[F, (F :+: G)#R] =
    new Member[F, (F :+: G)#R] {
      def inject[A](fa: F[A]): (F :+: G)#R[A] = Inl(fa)
    }

  implicit def right[F[_], G[_], H[_]](implicit member: Member[F, H]): Member[F, (G :+: H)#R] =
    new Member[F, (G :+: H)#R] {
      def inject[A](fa: F[A]): (G :+: H)#R[A] = Inr(member.inject(fa))
    }
}
```

左側への埋め込みは `Member.left` が、右側への埋め込みは `Member.right` が行います。

先の例で Member を利用すると次のようになります。

```scala
val u2 = implicitly[Member[Option, (List :+: (Option :+: Void)#R)#R]].inject(Some(0))
```

暗黙的な Member のインスタンスを使うことで、型に合わせて `Inl` と `Inr` を書かずに Union の値を得ることができました。

Union を使った Freer を Eff と呼ぶことにします。

Eff は次のように定義されます。

```scala
sealed trait Eff[R[_], A] {
  def map[B](f: A => B): Eff[R, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(r, k) => Impure(r, k :+ f)
    }
}

case class Pure[R[_], A](a: A) extends Eff[R, A]

case class Impure[R[_], A, B](union: R[A], k: Arrows[R, A, B]) extends Eff[R, B]
```

これは名前が違うだけで Freer の定義と同等であることがわかります。

また、これから型パラメータ `R` を `エフェクトスタック` と呼ぶことにします。

Member を使うことで副作用のある計算は次のように構築できます。

```scala
object Eff {
  def apply[R[_], F[_], A](fa: F[A])(implicit F: Member[F, R]): Eff[R, A] =
    Impure(F.inject(fa), Leaf((x: A) => Pure(x)))
}
```

Member の制約を使って副作用付きの計算 `F` をエフェクトスタック `R` に埋め込むことで `F[A]` から `Eff[R, A]` を作ります。

これらを用いて Writer モナドを定義してみましょう。

Writer モナドは計算値とは別に出力の値をもちます。

```scala
for {
  _ <- tell("hello, ")
  _ <- tell("world.")
} yield 0
```

例えばこのような式は `("hello, world.", 0)` のような値を返します。

Writer は次のような構造をもちます。

```scala
sealed trait Writer[+A]

case class Tell(value: String) extends Writer[Unit]

def tell[R[_]](value: String)(implicit w: Member[Writer, R]): Eff[R, Unit] = Eff(Tell(value))
```

今回は説明を簡略化するために String のみを出力できるようにしています。

Writer は計算値 `A` を型パラメータにもちます。

`Tell` は出力 `value` を値にもち、`Writer[Unit]` を継承します。

これは `Tell` という構造が計算値 `Unit` を返すことを意味します。

`tell` は `value` を出力するメソッドで、エフェクトスタック `R` が Writer を含む制約を Member で表現しています。

次はこの Writer を含む Eff を実行して出力値と計算値をとりだすハンドラを記述します。

```scala
object Writer {
  def run[R[_], A](eff: Eff[(Writer :+: R)#R, A]): Eff[R, (String, A)] =
    eff match {
      case Pure(a) => Pure(("", a))
      case Impure(Inl(Tell(v)), k) => run(k(())).map { case (s, a) => (v + s, a) }
      case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a))))
    }
}
```

1行ずつ見ていきましょう。

`Writer.run` はエフェクトスタックの先頭に Writer を含む Eff を受け取り、エフェクトスタックから Writer を取り除いて実行結果 `(String, A)` をもつ Eff を返します。

`eff` が Pure の場合は空の出力 `""` と計算値 `a` のペアで結果を返します。

`eff` が Impure でかつ Inl の場合、つまり Writer のエフェクトが含まれるとき Writer の唯一のインスタンスである `Tell` にマッチします。

Tell は `Writer[Unit]` を継承するので、継続 `k` に渡せる計算値は `Unit` に固定されます。

継続 `k` を実行した結果を `run` で再帰的に実行し、その最終的な計算結果に対して `map` を使って出力値 `v` を加えます。

`eff` が Impure でかつ Inr の場合、つまり Writer 以外のエフェクト `r` のとき Impure でそのまま返します。

このとき、Impure の継続 `k` を `run` で再帰的に実行します。

このハンドラの定義はある程度パターン化されているので、型を合わせることで自然に定義することが可能です。

Eff のエフェクトスタック `R` は漸減し、最終的に Void になります。

Eff から値を取り出す関数は次のように定義されます。

```scala
object Eff {
  def run[A](eff: Eff[Void, A]): A =
    eff match {
      case Pure(a) => a
    }
}
```

Void のインスタンスが存在しないことから、`Eff[Void, A]` は Impure を値にもたないため安全に実行されます。

Eff で表現された Writer モナドは次のように利用することができます。

```scala
def e1[R[_]](implicit w: Member[Writer, R]) = for {
  _ <- tell("hello, ")
  _ <- tell("world.")
} yield 0

assert(Eff.run(Writer.run(e1)) == ("hello, world.", 0))
```

Eff を使うプログラムはエフェクトスタック `R` を型パラメータにとり、`R` に対して Member で利用するエフェクトを制約に加える必要があります。

このプログラムは Writer のみ利用しているので最終的なエフェクトスタックは `Writer :+: Void` になり、それぞれ `Writer.run` と `Eff.run` で実行されます。

Maybe モナドも Eff で同様に定義してみましょう。

```scala
case class Maybe[A]()

def some[R[_], A](a: A): Eff[R, A] = Pure(a)
def none[R[_], A](implicit m: Member[Maybe, R]): Eff[R, A] = Eff(Maybe[A])

object Maybe {
  def run[R[_], A](eff: Eff[(Maybe :+: R)#R, A])(default: A): Eff[R, A] =
    eff match {
      case Pure(a) => Pure(a)
      case Impure(Inl(Maybe()), _) => Pure(default)
      case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a))(default)))
    }
}
```

`some` は Pure、`none` は `Maybe` を含む Impure で表現されます。

`Maybe.run` はエフェクトスタックの先頭に Maybe を含む Eff とデフォルト値 `default` を受け取り、エフェクトスタックから Maybe を取り除いて実行結果 `A` をもつ Eff を返します。

`eff` が Pure の場合は値が存在するということで値をそのまま返します。

`eff` が Impure で Inl をもつならば、継続を実行せずにデフォルト値を返します。

`eff` が Impure で Inr をもつならば、その継続 `k` を `run` で再帰的に実行します。

Eff で表現された Maybe モナドは次のように利用することができます。

```scala
def e2[R[_]](implicit m: Member[Maybe, R]) = for {
  x <- some(2)
  y <- none[R, Int]
} yield x + y

assert(Eff.run(Maybe.run(-1)(e2)) == -1)
```

`none` が含まれる式を実行するとデフォルト値が返ります。

Eff ではこれらのモナドを一つのfor式で混合させることができます。

```scala
def e3[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
  for {
    _ <- tell("hello, ")
    _ <- none[R, Unit]
    _ <- tell("world.")
  } yield 0

assert(Eff.run(Writer.run(Maybe.run(-1)(e3))) == ("hello, ", -1))
```

`e3` は Writer モナドと Maybe モナドが混在しています。

これをエフェクトスタック `Maybe :+: Writer :+: Void` で実行すると、最初の `tell` は成功しますが、次の `tell` は `none` により継続が破棄されています。

エフェクトの実行順序は自由に変えることができます。

```scala
def e4[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
  for {
    _ <- tell("hello, ")
    _ <- none[R, Unit]
    _ <- tell("world.")
  } yield 0

assert(Eff.run(Maybe.run(("fail", -1))(Writer.run(e4))) == ("fail", -1))
```

`e3` が Maybe から Writer の順で実行していたのに対し、`e4` は Writer から実行しています。

Maybe が最後に実行されることで全体の結果がデフォルト値になります。

ここまでで Eff についてまとめると次のようになります。

* 様々なモナドを表現できる
* 複数の作用を混在させることが可能
* モナドの合成が高速
* モナドの実行順序を自由に決められる

他にもエフェクトスタックを跨いだ処理が可能などの利点がありますが、[原論文](http://okmij.org/ftp/Haskell/extensible/more.pdf)や論文の日本語による[解説](https://www.slideshare.net/konn/freer-monads-more-extensible-effects-59411772)を参照してください。

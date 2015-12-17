---
layout: default
title: スタックレスScala
---

# スタックレスScala

[Stackless Scala With Free Monads](http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf)を参考にTrampolineやそれを抽象化したFreeを紹介します。

## Abstract

Scalaコンパイラの末尾再帰除去は自分自身を呼び出すメソッドのみに限定されます。

今回はどのような再帰呼び出しでもスタックを消費しないようにする方法を紹介します。

## Introduction

Scalaでプログラムを書いて`java.lang.StackOverflowError`に遭遇した経験があることでしょう。

```scala
def factorial(n: BigInt): BigInt =
  if (n <= 1)
    1
  else
    n * factorial(n - 1)
```

これは階乗の計算です。

```scala
scala> factorial(10000)
java.lang.StackOverflowError
	at java.math.BigInteger.subtract(BigInteger.java:1425)
	at scala.math.BigInt.$minus(BigInt.scala:207)
	at .factorial(<console>:11)
        at .factorial(<console>:11)
	at .factorial(<console>:11)
```

大きな数を与えると`StackOverflowError`が投げられます。

## Background: Tail-call elimination in Scala

末尾で自身を呼び出す関数`foldl`は次のようになります。

```scala
def foldl[A, B](as: List[A], b: B, f: (B, A) => B): B =
  as match {
    case Nil => b
    case x :: xs => foldl(xs, f(b, x), f)
  }
```

これは`var`と`while`を使ったコードに機械的に変換可能です。

コンパイルされたコードは以下と同等です。

```scala
def foldl[A, B](as: List[A], b: B, f: (B, A) => B): B = {
  var b_ = b
  var as_ = as
  while (true) {
    as_ match {
      case Nil => return b_
      case x :: xs =>
        as_ = xs
        b_ = f(b_, x)
    }
  }
  b_
}
```

どのような末尾呼び出しでも最適化されるのでしょうか？

```scala
def even(n: Int): Boolean =
  if (n <= 0)
    true
  else
    odd(n - 1)

def odd(n: Int): Boolean =
  if (n <= 0)
    false
  else
    even(n - 1)
```

これは`even`と`odd`を末尾で相互に呼び出しています。

```scala
scala> even(100000)
java.lang.StackOverflowError
	at .odd(<console>:14)
	at .even(<console>:11)
	at .odd(<console>:17)
	at .even(<console>:11)
	at .odd(<console>:17)
	at .even(<console>:11)
```

相互再帰では最適化が行われません。

これらの問題を解決するデータ構造が存在します。

## Tampolines: Trading stack for heap

`Trampoline`の定義は次のようになります。

```scala
sealed trait Trampoline[+A] {
  final def runT: A =
    this match {
       case More(k) => k().runT
       case Done(v) => v
    }
}

case class Done[A](a: A) extends Trampoline[A]

case class More[A](k: () => Trampoline[A]) extends Trampoline[A]
```

`runT`は再帰的に次のステップを呼び出し、結果を得ます。

`runT`の呼び出しは末尾であり、最適化が可能です。

`Trampoline`を用いた相互再帰は次のように書けます。

```scala
def even(n: Int): Trampoline[Boolean] =
  if (n <= 0)
    Done(true)
  else
    More(() => odd(n - 1))

def odd(n: Int): Trampoline[Boolean] =
  if (n <= 0)
    Done(false)
  else
    More(() => even(n - 1))
```

```scala
scala> even(100000)
res0: Trampoline[Boolean] = More(<function0>)

scala> .runT
res1: Boolean = true

```

## Making every call a tail cal

最初に挙げた例を解決出来るでしょうか？

```scala
def factorial(n: BigInt): Trampoline[BigInt] =
  if (n <= 1)
    Done(1)
  else
    More(() => Done(n * factorial(n - 1).runT))
```

```scala
scala> factorial(10000)
res0: Trampoline[BigInt] = More(<function0>)

scala> .runT
java.lang.StackOverflowError
	at $anonfun$factorial$1.apply(<console>:16)
	at $anonfun$factorial$1.apply(<console>:16)
	at Trampoline$class.runT(<console>:10)
	at More.runT(<console>:18)
	at $anonfun$factorial$1.apply(<console>:16)
	at $anonfun$factorial$1.apply(<console>:16)
	at Trampoline$class.runT(<console>:10)
	at More.runT(<console>:18)
```

関数内で`runT`を呼び出してしまっているため、スタックを消費しています。

### A Trampoline monad?

`Trampoline`をモナドにすることで解決を試みます。

単純に実装すると次のようになります。

```scala
def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
  More(() => f(runT))
```

しかし、`flatMap`内で`runT`を呼び出してしまうと先ほどと同じ結果になってしまいます。

### Building the monad right in

ここでは`Trampoline`にコンストラクタを追加します。

```scala
case class FlatMap[A, B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]
```

`flatMap`、`map`は次のように定義できます。

```scala
def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
  this match {
    case a FlatMap g => FlatMap(a, (x: Any) => g(x) flatMap f)
    case x => FlatMap(x, f)
  }

def map[B](f: A => B): Trampoline[B] =
  flatMap(a => Done(f(a)))
```

コンストラクタを追加したことで`runT`に変更を加える必要があります。

新しい`runT`は次に示す`resume`メソッドによって定義されます。

```scala
final def resume: Either[() => Trampoline[A], A] =
  this match {
    case Done(a) => Right(a)
    case More(k) => Left(k)
    case a FlatMap f => a match {
      case Done(a) => f(a).resume
      case More(k) => Left(() => k() flatMap f)
      case b FlatMap g => b.flatMap((x: Any) => g(x) flatMap f).resume
    }
  }
```

`resume`メソッドは`FlatMap`を適用して結果か次のステップを返します。
 
これはコンストラクタが`Done`と`More`の2つの時の`Trampoline`と同じです。

よって、`runT`は以下の様に書くことが出来ます。

```scala
final def runT: A = resume match {
  case Right(a) => a
  case Left(k) => k().runT
}
```

`resume`、`runT`は末尾で自身を呼び出しているので、このメソッドはコンパイラによって最適化されます。

### Stackless Scala

`flatMap`、`map`が定義されたことによって最初の例は次のようになります。

```scala
def factorial(n: BigInt): Trampoline[BigInt] =
  if (n <= 1)
    Done(1)
  else
    More(() => factorial(n - 1).map(n * _))
```

```scala
scala> factorial(10000)
res0: Trampoline[BigInt] = More(<function0>)

scala> .runT
res1: BigInt = 28462596809170545189064132121198688901480514017027992307941799942744113400037644437729907867577847758158840621423175288300423399401535187390524211613827161748198241998275924182892597878981242531205946599625986706560161572036032397926328736717055741975962099479720346153698119897092611277500484198845410475544642442136573303076703628825803548967461117097369578603670191071512730587281041158640561281165385325968425825995584688146430425589836649317059251717204276597407446133400054194052462303436869154059404066227828248371512038322178644627183822923899638992827221879702459387693803094627332292570555459690027875282242544348021127559019169425429028916907219097083690539873747452483372899521802363282741217040268086769210451555840567172555372015852132829034279989818449313610640381489...
```

もう一つ例を示します。

```scala
def fib(n: Int): Int =
  if (n < 2)
    n
  else
    fib(n - 1) + fib(n - 2)
```

末尾で呼び出しているのは`+`のため、最適化が行われません。

`Trampoline`とfor式を用いると自然な形で記述することが出来ます。

```scala
def fib(n: Int): Trampoline[Int] =
  if (n < 2)
    Done(n)
  else
    for {
      x <- More(() => fib(n - 1))
      y <- More(() => fib(n - 2))
    } yield x + y
```

## Free Monads: A Generalization of Trampoline

`Trampoline`は`Function0`を利用しています。

この`Function0`の部分を抽象化すると次のような定義が可能です。

```scala
sealed trait Free[S[+_], +A]

case class Done[S[+_], A](a: A) extends Free[S, A]

case class More[S[+_], A](k: S[Free[S, A]]) extends Free[S, A]

case class FlatMap[S[+_], A, B](a: Free[S, A], f: A => Free[S, B]) extends Free[S, B]
```

`Trampoline`は`Free`を用いて以下のように定義出来ます。

```scala
type Trampoline[+A] = Free[Function0, A]
```

`Function0`を抽象化したことによって、`resume`を変更する必要があります。

実は、`resume`は`Function0`を`Functor`として利用することで定義が可能です。

```scala
trait Functor[F[_]] {
  def map[A, B](m: F[A])(f: A => B): F[B]
}
```

`Function0Functor`は次のような定義になります。

```scala
implicit val f0Functor: Functor[Function0] =
  new Functor[Function0] {
    def map[A, B](a: () => A)(f: A => B): () => B =
      () => f(a())
  }
```

### Functions deﬁned on all free monads

`resume`は`Functor`を利用して次のように定義出来ます。

```scala
final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] =
  this match {
    case Done(a) => Right(a)
    case More(k) => Left(k)
    case a FlatMap f => a match {
      case Done(a) => f(a).resume
      case More(k) => Left(S.map(k)(_ flatMap f))
      case b FlatMap g => b.flatMap((x: Any) => g(x) flatMap f).resume
    }
  }
```

### Common data types as free monads

`Free`で表現出来るデータ型は`Trampoline`だけではありません。

`Free[S, A]`の`S`を枝、`A`を葉と見做すことで木構造を表現出来ます。

```scala
type Pair[+A] = (A, A)

type BinTree[+A] = Free[Pair, A]
```

この場合は枝は`Tuple2`、葉は`A`で二分木を表現しています。

`Pair`に対して2つの要素に関数を適用するような`Functor`を定義すれば、`BinTree`は全ての葉を走査するような`Monad`が定義されます。

### A free State monad

最後に、`Free`を使ったプログラミングについて話します。

ここでは例として`State`を構築します。

まず最初に、枝となるデータ型を定義します。

```scala
sealed trait StateF[S, +A]

case class Get[S, A](f: S => A) extends StateF[S, A]

case class Put[S, A](s: S, a: A) extends StateF[S, A]
```

ここで大切なことは関数のモデルを`case class`と`object`表現することです。

次にFunctorを定義します。

```scala
implicit def statefFunctor[S]: Functor[({ type F[A] = StateF[S, A] })#F] =
  new Functor[({ type F[A] = StateF[S, A] })#F] {
    def map[A, B](m: StateF[S, A])(f: A => B): StateF[S, B] =
      m match {
        case Get(g) => Get((s: S) => f(g(s)))
        case Put(s, a) => Put(s, f(a))
      }
  }
```

Functor則に気を付ければ自然と`map`を定義することが可能です。

`StateF`を使った`FreeState`の定義は以下のようになります。

```scala
type FreeState[S, +A] = Free[({ type F[+B] = StateF[S, B] })#F, A]
```

`FreeState`を返す関数として、次のようなものが定義出来ます。

```scala
def pureState[S, A](a: A): FreeState[S, A] =
  Done[({ type F[+B] = StateF[S, B] })#F, A](a)

def getState[S]: FreeState[S, S] =
  More[({ type F[+B] = StateF[S, B] })#F, S](Get(s => pureState(s)))

def setState[S](s: S): FreeState[S, Unit] =
  More[({ type F[+B] = StateF[S, B] })#F, Unit](Put(s, pureState(())))
```

そして、最初に定義した関数のモデルの実装は以下のように定義されます。

```scala
def evalS[S, A](s: S, t: FreeState[S, A]): A =
  t.resume match {
    case Left(Get(f)) => evalS(s, f(s))
    case Left(Put(n, a)) => evalS(n, a)
    case Right(a) => a
  }
```

`evalS`は末尾で自身を呼び出しており、コンパイラによって最適化されます。

このように、`resume`を使って計算を進めることでスタックを消費しない関数を定義することが可能です。

## Conclusions

スタックの代わりにヒープを使うことで再帰呼び出しを行うTrampolineを紹介しました。

Freeに一般化することで、その利点を他のモナドで生かすことができます。

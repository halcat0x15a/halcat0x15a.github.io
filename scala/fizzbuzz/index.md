---
layout: article
title: Typelevel FizzBuzz in Scala
---

# Typelevel FizzBuzz in Scala

Scala の型レベルプログラミングを FizzBuzz により解説します。

## 型レベルプログラミング

型システム上で計算を行う手法のことで、以下のような利点があります。

* コンパイル時計算
* 型システムによる計算の保証

## 型レベル自然数の定義

型レベルの自然数は `Zero` と `Succ` により帰納的に定義できます。

```scala
trait Nat
trait Zero extends Nat
trait Succ[N <: Nat] extends Nat
```

0 から 15 までの自然数は `Nat` を使って以下のように表すことができます。

```scala
type _0 = Zero
type _1 = Succ[_0]
type _2 = Succ[_1]
type _3 = Succ[_2]
type _4 = Succ[_3]
type _5 = Succ[_4]
type _6 = Succ[_5]
type _7 = Succ[_6]
type _8 = Succ[_7]
type _9 = Succ[_8]
type _10 = Succ[_9]
type _11 = Succ[_10]
type _12 = Succ[_11]
type _13 = Succ[_12]
type _14 = Succ[_13]
type _15 = Succ[_14]
```

ここで、型レベルの自然数 `Nat` を `Int` の値へ変換することを考えましょう。

`ToInt` は型レベルの自然数 `N` に対し `Int` を対応させます。

```scala
trait ToInt[N <: Nat] { def apply(): Int }

object ToInt {
  def apply[N <: Nat](implicit toInt: ToInt[N]): Int = toInt()
}
```

`ToInt.apply` は型レベルの自然数 `N` により決定される `ToInt` のインスタンスの `apply` メソッドを呼び出しています。

`ToInt` のインスタンスは次のように定義できます。

```scala
implicit def zero: ToInt[Zero] =
  new ToInt[Zero] {
    def apply(): Int = 0
  }

implicit def succ[N <: Nat](implicit toInt: ToInt[N]): ToInt[Succ[N]] =
  new ToInt[Succ[N]] {
    def apply(): Int = toInt() + 1
  }
```

`ToInt[Zero]` は `0` を返します。

`ToInt[Succ[N]]` は `ToInt[N]` に `1` 加えた値を返します。

`ToInt` は次のように動作します。

```scala
assert(ToInt[_0] == 0)
assert(ToInt[_9] == 9)
```

## 演算の定義

FizzBuzz を計算する為には `Nat` に対して剰余を定義する必要があります。

剰余の定義の為に型レベルの四則演算を定義しましょう。

```scala
trait Plus[N <: Nat, M <: Nat] {
  type Result <: Nat
}

trait Minus[N <: Nat, M <: Nat] {
  type Result <: Nat
}

trait Mult[N <: Nat, M <: Nat] {
  type Result <: Nat
}

trait Div[N <: Nat, M <: Nat] {
  type Result <: Nat
}
```

四則演算は自然数 `N`, `M` に対して計算結果 `Result` を対応させます。

これらの演算には次のようなエイリアスがあると便利です。

```scala
type +[N <: Nat, M <: Nat] = Plus[N, M]
type -[N <: Nat, M <: Nat] = Minus[N, M]
type *[N <: Nat, M <: Nat] = Mult[N, M]
type /[N <: Nat, M <: Nat] = Div[N, M]

type ==[E, R] = E { type Result = R }
```

除算 `Div` の定義の為に比較演算 `LT` を定義します。

```scala
trait LT[N <: Nat, M <: Nat]

type <[N <: Nat, M <: Nat] = LT[N, M]
```

`LT` のインスタンスは次のように定義できます。

```scala
object LT {
  implicit def zero[N <: Nat]: LT[Zero, Succ[N]] = new LT[Zero, Succ[N]] {}
  implicit def succ[N <: Nat, M <: Nat](implicit lt: LT[N, M]): LT[Succ[N], Succ[M]] = new LT[Succ[N], Succ[M]] {}
}
```

`Zero < Succ[N]` は真です。

`N < M` ならば `Succ[N] < Succ[M]` は真です。

`LT` の動作は次のように確認できます。

```scala
implicitly[_2 < _3]
implicitly[_3 < _6]
```

`Plus` のインスタンスは次のように定義できます。

```scala
object Plus {
  implicit def zero[N <: Nat]: Plus[N, Zero] { type Result = N } =
    new Plus[N, Zero] {
      type Result = N
    }

  implicit def succ[N <: Nat, M <: Nat](implicit plus: Plus[N, M]): Plus[N, Succ[M]] { type Result = Succ[plus.Result] } =
    new Plus[N, Succ[M]] {
      type Result = Succ[plus.Result]
    }
}
```

`N + Zero` は `N` を結果とします。

`N + M` が `R` を結果とするならば `N + Succ[M]` は `Succ[R]` を結果とします。

`Plus` の動作は次のように確認できます。

```scala
implicitly[_2 + _3 == _5]
implicitly[_6 + _9 == _15]
```

`Minus` のインスタンスは次のように定義できます。

```scala
object Minus {
  implicit def zero[N <: Nat]: Minus[N, Zero] { type Result = N } =
    new Minus[N, Zero] {
      type Result = N
    }
  implicit def succ[N <: Nat, M <: Nat](implicit minus: Minus[N, M]): Minus[Succ[N], Succ[M]] { type Result = minus.Result } =
    new Minus[Succ[N], Succ[M]] {
      type Result = minus.Result
    }
}
```

`N - Zero` は `N` を結果とします。

`N - M` が `R` を結果とするならば `Succ[N] - Succ[M]` は `R` を結果とします。

`Minus` の動作は次のように確認できます。

```scala
implicitly[_6 - _4 == _2]
implicitly[_9 - _6 == _3]
```

`Mult` のインスタンスは次のように定義できます。

```scala
object Mult {
  implicit def zero[N <: Nat]: Mult[N, Zero] { type Result = Zero } =
    new Mult[N, Zero] {
      type Result = Zero
    }

  implicit def one[N <: Nat]: Mult[N, Succ[Zero]] { type Result = N } =
    new Mult[N, Succ[Zero]] {
      type Result = N
    }

  implicit def succ[N <: Nat, M <: Nat, R <: Nat](implicit mult: Mult[N, Succ[M]] { type Result = R }, plus: Plus[N, R]): Mult[N, Succ[Succ[M]]] { type Result = plus.Result } =
    new Mult[N, Succ[Succ[M]]] {
      type Result = plus.Result
    }
}
```

`N * Zero` は `Zero` を結果とします。

`N * Succ[Zero]` は `N` を結果とします。

`N * M` が `R` を結果とするならば `N * Succ[M]` は `N + R` を結果とします。

`Mult` の動作は次のように確認できます。

```scala
implicitly[_2 * _0 == _0]
implicitly[_3 * _2 == _6]
```

`Div` のインスタンスは次のように定義できます。

```scala
object Div {
  implicit def zero[N <: Nat, M <: Nat](implicit lt: LT[N, M]): Div[N, M] { type Result = Zero } =
    new Div[N, M] {
      type Result = Zero
    }

  implicit def succ[N <: Nat, M <: Nat, R <: Nat](implicit minus: Minus[N, M] { type Result = R }, div: Div[R, M]): Div[N, M] { type Result = Succ[div.Result] } =
    new Div[N, M] {
      type Result = Succ[div.Result]
    }
}
```

`N < M` ならば `N / M` は `Zero` を結果とします。

`N - M` が `R` を結果とするならば `N / M` は `Succ[R / M]` を結果とします。

`Div` の動作は次のように確認できます。

```scala
implicitly[_6 / _4 == _1]
implicitly[_9 / _3 == _3]
```

この四則演算を使って剰余は次のように定義できます。

```scala
trait Mod[N <: Nat, M <: Nat] {
  type Result <: Nat
}

type %[N <: Nat, M <: Nat] = Mod[N, M]

object Mod {
  implicit def mod[N <: Nat, M <: Nat, Q <: Nat, R <: Nat](implicit div: Div[N, M] { type Result = Q }, mult: Mult[Q, M] { type Result = R }, minus: Minus[N, R]): Mod[N, M] { type Result = minus.Result } =
    new Mod[N, M] {
      type Result = minus.Result
    }
}
```

`(N / M) * M` の結果が `R` ならば `N % M` は `N - R` を結果とします。

`Mod` の動作は次のように確認できます。

```scala
implicitly[_3 % _2 == _1]
implicitly[_8 % _3 == _2]
implicitly[_1 % _3 == _1]
```

## FizzBuzz

FizzBuzz は次のように定義します。

```scala
trait FizzBuzz[N <: Nat] {
  def apply(): String
}

object FizzBuzz {
  def apply[N <: Nat](implicit lt: LT[_0, N], fizzbuzz: FizzBuzz[N]): String = fizzbuzz()
}
```

`FizzBuzz` は自然数 `N` に対応する `String` の値を持ちます。

`FizzBuzz` のインスタンスは次のように定義できます。

```scala
implicit def fizzbuzz[N <: Nat](implicit mod: Mod[N, _15] { type Result = _0 }): FizzBuzz[N] =
  new FizzBuzz[N] {
    def apply(): String = "FizzBuzz"
  }
```

ここで `"Fizz"` と `"Buzz"` に対するインスタンスを定義すると、`"FizzBuzz"` に対するのインスタンスとの間でコンフリクトが発生します。

そこで、implicit parameter の解決の為に階層を構築します。

```scala
trait FizzBuzzLowestPriorityImplicits {
  implicit def number[N <: Nat](implicit toInt: ToInt[N]): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = toInt().toString
    }
}

trait FizzBuzzLowerPriorityImplicits extends FizzBuzzLowestPriorityImplicits {
  implicit def fizz[N <: Nat](implicit mod: Mod[N, _3] { type Result = _0 }): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = "Fizz"
    }
  implicit def buzz[N <: Nat](implicit mod: Mod[N, _5] { type Result = _0 }): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = "Buzz"
    }
}

object FizzBuzz extends FizzBuzzLowerPriorityImplicits {
  implicit def fizzbuzz[N <: Nat](implicit mod: Mod[N, _15] { type Result = _0 }): FizzBuzz[N] =
    new FizzBuzz[N] {
      def apply(): String = "FizzBuzz"
    }
}
```

この定義により、`FizzBuzz` から `FizzBuzzLowerPriorityImplicits`, `FizzBuzzLowestPriorityImplicits` の順に implicit が解決されます。

`FizzBuzz` は次のように動作します。

```scala
assert(FizzBuzz[_2] == "2")
assert(FizzBuzz[_3] == "Fizz")
assert(FizzBuzz[_5] == "Buzz")
assert(FizzBuzz[_15] == "FizzBuzz")
```

これで型レベルFizzBuzzは完成です。

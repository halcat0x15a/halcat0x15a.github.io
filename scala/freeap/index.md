---
layout: default
title: Free Applicative Functors in Scala
---

# Free Applicative Functors in Scala

[Free Applicative Functors](http://arxiv.org/abs/1403.0749)をScalaで紹介します。

## Applicative

ApplicativeはMonadをより一般化したものです。

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}
```

`ap`は`F`の文脈で`f`を`fa`に適用する関数です。

`ap`を利用することで任意のarityの関数を適用することができます。

```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] =
  F.ap(fb)(F.map(fa)(a => f(a, _)))

def map3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] =
  F.ap(fc)(map2(fa, fb)((a, b) => f(a, b, _)))
```

これはMonadより軽量な記述を可能にします。

```scala
for {
  a <- fa
  b <- fb
} yield f(a, b)
```

```scala
map2(fa, fb)(f)
```

## Free Monad

FreeはHaskellではとてもよく知られているデータ構造です。

```scala
sealed trait Free[F[_], A]

case class Pure[F[_], A](a: A) extends Free[F, A]

case class Impure[F[_], A](f: F[Free[F, A]]) extends Free[F, A]
```

しかし、Freeで構築した計算の構造は実行するまでわかりません。

## Free Applicative

Free Applicativeには2通りの実装があり、それらが同型であることが証明されています。

```scala
sealed trait Free[F[_], A]

case class Pure[F[_], A](a: A) extends Free[F, A]

case class Apply[F[_], A, B](f: F[B], k: Free[F, B => A]) extends Free[F, A]
```

Free MonadがTreeのような構造であることに対し、Free ApplicativeはListのような構造であることがわかります。

`Free`はApplicativeのインスタンスをもちます。

```scala
implicit def free[F[_]]: Applicative[({ type G[A] = Free[F, A] })#G] =
  new Applicative[({ type G[A] = Free[F, A] })#G] {
    def pure[A](a: A): Free[F, A] = Pure(a)
    override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] =
      fa match {
        case Pure(a) => Pure(f(a))
        case Apply(h, t) => Apply(h, map(t)(f compose _))
      }
    def ap[A, B](fa: Free[F, A])(f: Free[F, A => B]): Free[F, B] =
      f match {
        case Pure(g) => map(fa)(g)
        case Apply(h, t) => Apply(h, ap(fa)(map(t)(g => (a: A) => g(_: Any)(a))))
      }
  }
```

`ap`の定義はListの結合演算の定義と類似しています。

## Example: Option parser

Free Monadより有用なFree Applicativeの例を紹介します。

次のようなコマンドラインツールのオプションパーサ考えます。

```sh
create_user --username halcat0x15a --fullname SanshiroYoshida --id 346
```

`create_user`は`username`, `fullname`, `id`からユーザーを作成します。

```scala
case class User(username: String, fullname: String, id: Int)
```

ひとつのオプションは名前とデフォルト値と文字列から値を取得する関数をもちます。

```scala
case class Opt[A](name: String, default: Option[A], reader: String => Option[A])
```

`Opt`を`Free`に持ち上げましょう。

`one`は`F[A]`から`Free[F, A]`を構成します。

```scala
def one[F[_], A](fa: F[A]): Free[F, A] = Apply(fa, Pure((a: A) => a))
```

これらを用いて、パーサは次のように構築できます。

```scala
def username: Free[Opt, String] = one(Opt("username", None, Some[String]))
def fullname: Free[Opt, String] = one(Opt("fullname", Some(""), Some[String]))
def id: Free[Opt, Int] = one(Opt("id", None, s => allCatch.opt(s.toInt)))

def user: Free[Opt, User] = map3(username, fullname, id)(User)
```

Free Monadを用いたDSLに比べ次のような利点があります。

* 作用が独立している
* 解析が可能である

## Left adjoint

Free ApplicativeはApplicativeの圏から自己関手の圏への忘却関手の左随伴です。

関手の圏において射は自然変換となります。

```scala
trait Nat[F[_], G[_]] {
  def apply[A]: F[A] => G[A]
}
```

`Nat[F, G]`は`F`の内部構造を保ちながら`G`へ変換します。

`one`を使って自己関手の圏の対象`F[A]`をApplicativeの圏の対象`Free[F, A]`に写すことができました。

`Free`が関手であるということは対象だけでなく射を写すことができます。

```scala
def lift[F[_], G[_]](f: Nat[F, G]): Nat[({ type H[A] = Free[F, A] })#H, ({ type H[A] = Free[G, A] })#H] =
  new Nat[({ type H[A] = Free[F, A] })#H, ({ type H[A] = Free[G, A] })#H] {
    def apply[A]: Free[F, A] => Free[G, A] = {
      case Pure(a) => Pure(a)
      case Apply(h, t) => Apply(f[Any](h), lift(f).apply[Any => A](t))
    }
  }
```

Free ApplicativeがApplicativeの圏から自己関手の圏への忘却関手の左随伴ということは、Applicativeの圏の対象`Free[F, A]`と`G[A]`の間の射の集合と自己関手の圏の対象`F[A]`と`G[A]`の間の射の集合が同型であることを意味します。

同型写像`raise`と`lower`は次のように定義できます。

```scala
def raise[F[_], G[_]](f: Nat[F, G])(implicit G: Applicative[G]): Nat[({ type H[A] = Free[F, A] })#H, G] =
  new Nat[({ type H[A] = Free[F, A] })#H, G] {
    def apply[A]: Free[F, A] => G[A] = {
      case Pure(a) => G.pure(a)
      case Apply(h, t) => G.ap(f[Any](h))(raise(f).apply[Any => A](t))
    }
  }

def lower[F[_], G[_]](f: Nat[({ type H[A] = Free[F, A] })#H, G]): Nat[F, G] =
  new Nat[F, G] {
    def apply[A]: F[A] => G[A] = f[A] compose one
  }
```

`raise`は`Free`の解析に利用されます。

```scala
def parserDefault: Nat[({ type F[A] = Free[Opt, A] })#F, Option] =
  raise(new Nat[Opt, Option] { def apply[A] = _.default })

def allOptions: Nat[({ type F[A] = Free[Opt, A] })#F, Option] =
  raise(new Nat[Opt, ({ type F[A] = List[String] })#F] { def apply[A] = _.name :: Nil })(Monoid.list.applicative)
```

`parserDefault`はパーサのオプションのデフォルト値を取得します。

`allOptions`はパーサのオプションの名前を全て列挙します。

これらを用いてオプションパーサを実行する関数を作ることができます。

```scala
def matchOpt[A](opt: String, value: String, parser: Free[Opt, A]): Option[Free[Opt, A]] =
  parser match {
    case Pure(_) => None
    case Apply(h, t) =>
      if (opt == s"--${h.name}")
        h.reader(value).map(a => t.map(_(a)))
      else
        matchOpt(opt, value, t).map(Apply(h, _))
  }

def runParser[A](parser: Free[Opt, A], args: String): Option[A] = runParser(parser, args.split("\\s+").toList)

def runParser[A](parser: Free[Opt, A], args: List[String]): Option[A] =
  args match {
    case opt :: value :: args => matchOpt(opt, value, parser).flatMap(runParser(_, args))
    case Nil => parserDefault[A](parser)
    case _ => None
  }
```

`matchOpt`は`parser`がもつオプションの`name`と一致するものに対して`reader`を適用して値を得ます。

`runParser`は`matchOpt`を用いて入力をパースし、入力がない場合は`parserDefault`でオプションのデフォルト値を使います。

オプションパーサは以下のように動作します。

```scala
scala> runParser(user, "--username halcat0x15a --fullname SanshiroYoshida --id 346")
res0: Option[User] = Some(User(halcat0x15a,SanshiroYoshida,346))

scala> runParser(user, "--id 346 --username halcat0x15a --fullname SanshiroYoshida")
res1: Option[User] = Some(User(halcat0x15a,SanshiroYoshida,346))

scala> runParser(user, "--username halcat0x15a --id 346")
res2: Option[User] = Some(User(halcat0x15a,,346))

scala> runParser(user, "--fullname SanshiroYoshida --id 346")
res3: Option[User] = None
```

入力が順序不同であることやデフォルト値を使っていることがわかります。

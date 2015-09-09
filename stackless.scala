package org.halcat.stackless

import scala.language.higherKinds

object _1 {

  def factorial(n: BigInt): BigInt =
    if (n <= 1)
      1
    else
      n * factorial(n - 1)

  def foldl[A, B](as: List[A], b: B, f: (B, A) => B): B =
    as match {
      case Nil => b
      case x :: xs => foldl(xs, f(b, x), f)
    }

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

  def fib(n: Int): Int =
    if (n < 2)
      n
    else
      fib(n - 1) + fib(n - 2)

}

object _2 {

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

  sealed trait Trampoline[+A] {

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

    final def runT: A = resume match {
      case Right(a) => a
      case Left(k) => k().runT
    }

    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
      this match {
        case a FlatMap g => FlatMap(a, (x: Any) => g(x) flatMap f)
        case x => FlatMap(x, f)
      }

    def map[B](f: A => B): Trampoline[B] =
      flatMap(a => Done(f(a)))

  }

  case class Done[A](a: A)
      extends Trampoline[A]

  case class More[A](k: () => Trampoline[A])
      extends Trampoline[A]

  case class FlatMap[A, B](sub: Trampoline[A], k: A => Trampoline[B])
      extends Trampoline[B]

  def factorial(n: BigInt): Trampoline[BigInt] =
    if (n <= 1)
      Done(1)
    else
      More(() => Done(n * factorial(n - 1).runT))

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

}

object _3 {

  import _2._

  def factorial(n: BigInt): Trampoline[BigInt] =
    if (n <= 1)
      Done(1)
    else
      More(() => factorial(n - 1).map(n * _))

  def fib(n: Int): Trampoline[Int] =
    if (n < 2)
      Done(n)
    else
      for {
        x <- More(() => fib(n - 1))
        y <- More(() => fib(n - 2))
      } yield x + y

}

object _4 {

  type Trampoline[+A] = Free[Function0, A]

  sealed trait Free[S[+_], +A] {

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

    def flatMap[B](f: A => Free[S, B]): Free[S, B] =
      this match {
        case a FlatMap g => FlatMap(a, (x: Any) => g(x) flatMap f)
        case x => FlatMap(x, f)
      }

    def map[B](f: A => B): Free[S, B] =
      flatMap(a => Done(f(a)))

  }

  case class Done[S[+_], A](a: A) extends Free[S, A]

  case class More[S[+_], A](k: S[Free[S, A]]) extends Free[S, A]

  case class FlatMap[S[+_], A, B](a: Free[S, A], f: A => Free[S, B]) extends Free[S, B]

  trait Functor[F[_]] {
    def map[A, B](m: F[A])(f: A => B): F[B]
  }

  implicit val f0Functor =
    new Functor[Function0] {
      def map[A, B](a: () => A)(f: A => B): () => B =
        () => f(a())
    }

  type Pair[+A] = (A, A)

  type BinTree[+A] = Free[Pair, A]

}

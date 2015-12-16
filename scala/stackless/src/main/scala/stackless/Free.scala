package stackless

import scala.language.higherKinds

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

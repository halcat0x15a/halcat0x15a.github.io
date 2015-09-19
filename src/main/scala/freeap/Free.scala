package freeap

import scala.language.higherKinds

trait Nat[F[_], G[_]] {
  def apply[A]: F[A] => G[A]
}

sealed trait Free[F[_], A] {

  def map[B](f: A => B): Free[F, B] =
    this match {
      case Pure(a) => Pure(f(a))
      case Apply(h, t) => Apply(h, t.map(f compose _))
    }

}

case class Pure[F[_], A](a: A) extends Free[F, A]

case class Apply[F[_], A, B](f: F[B], k: Free[F, B => A]) extends Free[F, A]

object Free {

  def one[F[_], A](fa: F[A]): Free[F, A] = Apply(fa, Pure((a: A) => a))

  def lift[F[_], G[_]](f: Nat[F, G]): Nat[({ type H[A] = Free[F, A] })#H, ({ type H[A] = Free[G, A] })#H] =
    new Nat[({ type H[A] = Free[F, A] })#H, ({ type H[A] = Free[G, A] })#H] {
      def apply[A]: Free[F, A] => Free[G, A] = {
        case Pure(a) => Pure(a)
        case Apply(h, t) => Apply(f[Any](h), lift(f).apply[Any => A](t))
      }
    }

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

}

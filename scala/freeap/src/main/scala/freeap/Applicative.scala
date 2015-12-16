package freeap

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}

object Applicative {

  def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] =
    F.ap(fb)(F.map(fa)(a => f(a, _)))

  def map3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] =
    F.ap(fc)(map2(fa, fb)((a, b) => f(a, b, _)))

  implicit val option: Applicative[Option] =
    new Applicative[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = f.flatMap(fa.map)
    }

  implicit val list: Applicative[List] =
    new Applicative[List] {
      def pure[A](a: A): List[A] = List(a)
      def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = f.flatMap(fa.map(_))
    }

}

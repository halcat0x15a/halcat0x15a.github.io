package stackless

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](m: F[A])(f: A => B): F[B]
}

object Functor {

  implicit val f0Functor =
    new Functor[Function0] {
      def map[A, B](a: () => A)(f: A => B): () => B =
        () => f(a())
    }

  implicit def statefFunctor[S]: Functor[({ type F[A] = StateF[S, A] })#F] =
    new Functor[({ type F[A] = StateF[S, A] })#F] {
      def map[A, B](m: StateF[S, A])(f: A => B): StateF[S, B] =
        m match {
          case Get(g) => Get((s: S) => f(g(s)))
          case Put(s, a) => Put(s, f(a))
        }
    }

}

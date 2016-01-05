package free

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

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

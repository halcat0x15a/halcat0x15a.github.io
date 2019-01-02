package freer

sealed trait Freer[F[_], A] {
  def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(fa, k) => Impure(fa, (a: Any) => k(a).flatMap(f))
    }
}

case class Pure[F[_], A](a: A) extends Freer[F, A]

case class Impure[F[_], A, B](fa: F[A], k: A => Freer[F, B]) extends Freer[F, B]

object Freer {

  def apply[F[_], A](ff: F[Freer[F, A]]): Freer[F, A] =
    Impure(ff, (x: Freer[F, A]) => x)

}

package free

case class Coyoneda[F[_], A, B](fa: F[A], k: A => B) {
  def map[C](f: B => C): Coyoneda[F, A, C] =
    Coyoneda(fa, k andThen f)
}

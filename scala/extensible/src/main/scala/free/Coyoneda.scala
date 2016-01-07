package free

trait Coyoneda[F[_], A, B] { self =>
  val fa: F[A]
  val k: A => B
  def map[C](f: B => C): Coyoneda[F, A, C] =
    new Coyoneda[F, A, C] {
      val fa: F[A] = self.fa
      val k: A => C = self.k andThen f
    }
}

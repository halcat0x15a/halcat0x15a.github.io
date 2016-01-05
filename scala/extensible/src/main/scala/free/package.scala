package object free {

  type Pair[A] = (A, A)

  implicit val PairFunctor: Functor[Pair] =
    new Functor[Pair] {
      def map[A, B](fa: Pair[A])(f: A => B): Pair[B] =
        fa match {
          case (x, y) => (f(x), f(y))
        }
    }

  type Tree[A] = Free[Pair, A]

  def leaf[A](a: A): Tree[A] = Pure(a)

  def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]])

}

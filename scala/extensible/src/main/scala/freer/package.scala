package object freer {
  type Pair[A] = (A, A)

  type Tree[A] = Freer[Pair, A]

  def leaf[A](a: A): Tree[A] = Pure(a)

  def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Freer((x, y): Pair[Tree[A]])

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Pure(a) => f(a)
      case Impure((x, y), k) => g(fold(k(x))(f)(g), fold(k(y))(f)(g))
    }

  def str[A](t: Tree[A]): String = fold(t)(_.toString)((x, y) => s"($x, $y)")

  type ConstUnit[A] = Unit

  type Maybe[A] = Freer[ConstUnit, A]

  def some[A](a: A): Maybe[A] = Pure(a)

  def none[A]: Maybe[A] = Freer((): ConstUnit[Maybe[A]])

  def maybe[A](maybe: Maybe[A])(default: A): A = maybe match {
    case Pure(a) => a
    case Impure((), _) => default
  }
}

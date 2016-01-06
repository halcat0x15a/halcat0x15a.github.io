package object extensible {

  type Tree[A] = (A, A)

  type Maybe[A] = Unit

  val tree1: Maybe :+: Tree :+: Void = Inr(Inl((0, 1): Tree[Int]))

  val tree2 = Member[Tree, Maybe :+: Tree :+: Void].inject((0, 1): Tree[Int])

  def leaf[U <: Union, A](a: A): Freer[U, A] = Pure(a)

  def node[U <: Union, A](x: Freer[U, A], y: Freer[U, A])(implicit member: Member[Tree, U]): Freer[U, A] = Freer((x, y): Tree[Freer[U, A]])

  def fold[U <: Union, A, B](t: Freer[Tree :+: U, A])(f: A => B)(g: (B, B) => B): Freer[U, B] =
    t match {
      case Pure(a) => Pure(f(a))
      case Impure(u, h) =>
        def k(t: Tree[Any]): Freer[U, B] =
          t match {
            case (x, y) =>
              for {
                a <- fold(h(x))(f)(g)
                b <- fold(h(y))(f)(g)
              } yield g(a, b)
          }
        u match {
          case Inl(t) => k(t)
          case Inr(u) => Impure(u, Leaf(k))
        }
    }

  def str[U <: Union, A, B](t: Freer[Tree :+: U, A]): Freer[U, String] = fold(t)(_.toString)((x, y) => s"($x, $y)")

  def nothing[U <: Union, A](implicit member: Member[Maybe, U]): Freer[U, A] = Freer((): Maybe[Freer[U, A]])

  def just[U <: Union, A](a: A): Freer[U, A] = Pure(a)

  def maybe[U <: Union, A](m: Freer[Maybe :+: U, A])(default: A): Freer[U, A] =
    m match {
      case Pure(a) => Pure(a)
      case Impure(u, _) =>
        def k(x: Any): Freer[U, A] = Pure(default)
        u match {
          case Inl(()) => k(())
          case Inr(u) => Impure(u, Leaf(k))
        }
    }

}

package object extensible {

  type Tree[A] = (A, A)

  type Maybe[A] = Unit

  val tree1: Maybe :+: Tree :+: Void = Inr(Inl((0, 1): Tree[Int]))

  val tree2 = Member[Tree, Maybe :+: Tree :+: Void].inject((0, 1): Tree[Int])

  def leaf[U <: Union, A](a: A)(implicit member: Member[Tree, U]): Eff[U, A] = Pure(a)

  def node[U <: Union, A](x: Eff[U, A], y: Eff[U, A])(implicit member: Member[Tree, U]): Eff[U, A] = Eff((x, y): Tree[Eff[U, A]])

  def fold[U <: Union, A, B](t: Eff[Tree :+: U, A])(f: A => B)(g: (B, B) => B): Eff[U, B] =
    t match {
      case Pure(a) => Pure(f(a))
      case Impure(u, h) =>
        def k(t: Tree[Any]): Eff[U, B] =
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

  def str[U <: Union, A, B](t: Eff[Tree :+: U, A]): Eff[U, String] = fold(t)(_.toString)((x, y) => s"($x, $y)")

  def nothing[U <: Union, A](implicit member: Member[Maybe, U]): Eff[U, A] = Eff((): Maybe[Eff[U, A]])

  def just[U <: Union, A](a: A)(implicit member: Member[Maybe, U]): Eff[U, A] = Pure(a)

  def maybe[U <: Union, A](m: Eff[Maybe :+: U, A])(default: A): Eff[U, A] =
    m match {
      case Pure(a) => Pure(a)
      case Impure(Inl(()), _) => Pure(default)
      case Impure(Inr(u), k) => Impure(u, Leaf((x: Any) => maybe(k(x))(default)))
    }

}

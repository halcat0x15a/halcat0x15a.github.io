package object extensible {
  type :+:[F[_], G[_]] = { type R[A] = Union[F, G, A] }

  val u1: (List :+: (Option :+: Void)#R)#R[Int] = Inr(Inl(Some(0)))

  val u2 = implicitly[Member[Option, (List :+: (Option :+: Void)#R)#R]].inject(Some(0))

  case class Maybe[A]()

  def some[R[_], A](a: A): Eff[R, A] = Pure(a)
  def none[R[_], A](implicit m: Member[Maybe, R]): Eff[R, A] = Eff(Maybe[A])

  object Maybe {
    def run[R[_], A](default: A)(eff: Eff[(Maybe :+: R)#R, A]): Eff[R, A] =
      eff match {
        case Pure(a) => Pure(a)
        case Impure(Inl(Maybe()), _) => Pure(default)
        case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(default)(k(a))))
      }
  }

  def e2[R[_]](implicit m: Member[Maybe, R]) = for {
    x <- some(2)
    y <- none[R, Int]
  } yield x + y

  assert(Eff.run(Maybe.run(-1)(e2)) == -1)


  sealed trait Writer[+A]
  case class Tell(value: String) extends Writer[Unit]
  def tell[R[_]](value: String)(implicit w: Member[Writer, R]): Eff[R, Unit] = Eff(Tell(value))
  object Writer {
    def run[R[_], A](eff: Eff[(Writer :+: R)#R, A]): Eff[R, (String, A)] =
      eff match {
        case Pure(a) => Pure(("", a))
        case Impure(Inl(Tell(v)), k) => run(k(())).map { case (s, a) => (v + s, a) }
        case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a))))
      }
  }

  def e1[R[_]](implicit w: Member[Writer, R]) = for {
    _ <- tell("hello, ")
    _ <- tell("world.")
  } yield 0

  assert(Eff.run(Writer.run(e1)) == ("hello, world.", 0))

  def e3[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
    for {
      _ <- tell("hello, ")
      _ <- none[R, Unit]
      _ <- tell("world.")
    } yield 0

  assert(Eff.run(Writer.run(Maybe.run(-1)(e3))) == ("hello, ", -1))

  def e4[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
    for {
      _ <- tell("hello, ")
      _ <- none[R, Unit]
      _ <- tell("world.")
    } yield 0

  assert(Eff.run(Maybe.run(("fail", -1))(Writer.run(e4))) == ("fail", -1))

  /*
  val u2 = Member[Tree, Maybe :+: Tree :+: Void].inject((0, 1): Tree[Int])

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
 */
}

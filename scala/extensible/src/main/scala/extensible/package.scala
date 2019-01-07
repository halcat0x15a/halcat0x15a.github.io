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
}

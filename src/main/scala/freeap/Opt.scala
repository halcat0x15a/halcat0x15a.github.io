package freeap

case class Opt[A](name: String, default: Option[A], reader: String => Option[A])

object Opt {

  def parserDefault: Nat[({ type F[A] = Free[Opt, A] })#F, Option] = Free.raise(new Nat[Opt, Option] { def apply[A] = _.default })

  type Options[A] = List[String]

  implicit val options: Applicative[Options] =
    new Applicative[Options] {
      def pure[A](a: A): Options[A] = Nil
      def ap[A, B](fa: Options[A])(f: Options[A => B]): Options[B] = f ::: fa
    }

  def allOptions: Nat[({ type F[A] = Free[Opt, A] })#F, ({ type F[A] = List[String] })#F] =
    Free.raise(new Nat[Opt, Options] { def apply[A] = _.name :: Nil })

  def matchOpt[A](opt: String, value: String, parser: Free[Opt, A]): Option[Free[Opt, A]] =
    parser match {
      case Pure(_) => None
      case Apply(h, t) =>
        if (opt == s"--${h.name}")
          h.reader(value).map(a => t.map(_(a)))
        else
          matchOpt(opt, value, t).map(Apply(h, _))
    }

  def runParser[A](parser: Free[Opt, A], args: String): Option[A] = runParser(parser, args.split("\\s+").toList)

  def runParser[A](parser: Free[Opt, A], args: List[String]): Option[A] =
    args match {
      case opt :: value :: args => matchOpt(opt, value, parser).flatMap(runParser(_, args))
      case Nil => parserDefault[A](parser)
      case _ => None
    }

}

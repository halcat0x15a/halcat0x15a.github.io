package stackless

sealed trait StateF[S, +A]

case class Get[S, A](f: S => A) extends StateF[S, A]

case class Put[S, A](s: S, a: A) extends StateF[S, A]

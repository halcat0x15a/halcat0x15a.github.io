package fpinscala.state

case class State[S, A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def map2[S, A, B, C](fa: State[S, A], fb: State[S, B])(f: (A, B) => C) =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil))((s, acc) => map2(s, acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

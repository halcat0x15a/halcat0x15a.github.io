package fpinscala.errorhandling

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }

  def orElse[EE >: E, AA >: A](that: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_) => that
      case Right(_) => this
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(a => b.map(b => f(a, b)))

}

case class Left[E](value: E) extends Either[E, Nothing]

case class Right[A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(Right(Nil): Either[E, List[B]])((acc, a) => acc.flatMap(xs => f(a).map(x => x :: xs)))

}

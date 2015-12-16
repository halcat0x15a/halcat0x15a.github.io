package fpinscala.errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def orElse[B >: A](that: => Option[B]): Option[B] = map(Some(_)).getOrElse(that)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case object None extends Option[Nothing]

case class Some[A](get: A) extends Option[A]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((opt, acc) => acc.flatMap(xs => opt.map(x => x :: xs)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((a, acc) => acc.flatMap(xs => f(a).map(x => x :: xs)))

}

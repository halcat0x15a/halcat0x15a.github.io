package fpinscala.laziness

sealed trait Stream[+A] {

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(head, tail) => head() :: tail().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Empty => Stream.empty
      case Cons(head, tail) =>
        if (n > 0)
          Stream.cons(head(), tail().take(n - 1))
        else
          Stream.empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Empty => Stream.empty
      case Cons(_, tail) =>
        if (n > 0)
          tail().drop(n - 1)
        else
          this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Stream.empty
      case Cons(head, tail) =>
        if (p(head()))
          Stream.cons(head(), tail().takeWhile(p))
        else
          Stream.empty
    }

  def foldRight[B](b: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(b)(f))
      case Empty => b
    }

  def forall(p: A => Boolean): Boolean =
    this match {
      case Empty => true
      case Cons(head, tail) => p(head()) && tail().forall(p)
    }

  def takeWhilef(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if (p(a)) Stream.cons(a, acc) else Stream.empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, acc) => Some(a))

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, acc) => Stream.cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => f(a).append(acc))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, acc) => Stream.cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, acc) => if (f(a)) Stream.cons(a, acc) else acc)

  def umap[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(head, tail) => Some((f(head()), tail()))
    }

  def utake(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Empty, _) => None
      case (Cons(head, tail), n) =>
        if (n > 0)
          Some((head(), (tail(), n - 1)))
        else
          None
    }

  def utakeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(head, tail) =>
        if (p(head()))
          Some((head(), tail()))
        else
          None
    }

  def zipWith[B, C](that: => Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, that)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(x, xs), Cons(y, ys)) => Some((f(x(), y()), (xs(), ys())))
    }

  def zipAll[B](that: => Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(head, tail), Empty) => Some(((Some(head()), None), (tail(), Empty)))
      case (Empty, Cons(head, tail)) => Some(((None, Some(head())), (Empty, tail())))
      case (Cons(x, xs), Cons(y, ys)) => Some(((Some(x()), Some(y()))), (xs(), ys()))
    }

  def startsWith[B >: A](prefix: => Stream[B]): Boolean =
    zipAll(prefix).forall {
      case (Some(x), Some(y)) => x == y
      case (None, _) => false
      case (_, None) => true
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case cons@Cons(_, tail) => Some((cons, tail()))
    }

  /*
  def foldRight[B](b: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(b)(f))
      case Empty => b
    }
   */
  //Stream(a, b, c, d).scanRight(e)(f) == Stream(f(a, f(b, f(c, f(d, e)))), f(b, f(c, f(d, e))), f(c, f(d, e)), f(d, e), e)
  def scanRight[S](s: S)(f: (A, => S) => S): Stream[S] =
    this match {
      case Empty => Stream(s)
      case Cons(head, tail) =>
        tail().scanRight(s)(f) match {
          case xs@Cons(x, _) => Stream.cons(f(head(), x()), xs)
          case Empty => Empty
        }
    }

  // SUM(1..n)
  def uscanRight[S](s: S)(f: (A, => S) => S): Stream[S] =
    Stream.unfold(this) {
      case Empty => None
      case cons@Cons(_, tail) => Some((cons.foldRight(s)(f), tail()))
    }.append(Stream(s))

}

case object Empty extends Stream[Nothing]

case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

  def empty[A]: Stream[A] = Empty

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = cons(0, cons(1, for (i <- from(0); n <- fibs.drop(i).take(1); m <- fibs.drop(i + 1).take(1)) yield n + m))
  //def fibs: Stream[Int] = from(0).map(fpinscala.gettingstarted.MyModule.fib)

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def ones: Stream[Int] = unfold(())(_ => Some((1, ())))

  def uconstant[A](a: A): Stream[A] = unfold(())(_ => Some((a, ())))

  def ufrom(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  def ufibs: Stream[Int] = unfold((0, 1)) { case (n, m) => Some((n, (m, n + m))) }

}

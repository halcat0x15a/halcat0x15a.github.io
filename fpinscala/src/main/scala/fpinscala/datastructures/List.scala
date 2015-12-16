package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => throw new UnsupportedOperationException("tail of empty list")
      case Cons(_, t) => t
    }

  def setHead[A](as: List[A], a: A): List[A] =
    as match {
      case Cons(_, t) => Cons(a, t)
      case _ => throw new UnsupportedOperationException("head of empty list")
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) =>
        if (n == 0)
          l
        else
          drop(t, n - 1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h))
          dropWhile(t, f)
        else
          l
    }

  def init[A](l: List[A]): List[A] =
    reverse(tail(reverse(l)))

  def sum(is: List[Int]): Int =
    foldLeft(is, 0)(_ + _)

  def product(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) => B): B =
    as match {
      case Nil => b
      case Cons(x, xs) => foldLeft(xs, f(b, x))(f)
    }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

  def length[A](l: List[A]): Int =
    foldLeft(l, 0)((n, _) => n + 1)

  def foldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), b)((b, a) => f(a, b))

  def append[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)((a, acc) => Cons(a, acc))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def flatten[A](ass: List[List[A]]): List[A] =
    foldRight(ass, Nil: List[A])((as, acc) => append(acc, as))

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def go(as: List[A], bs: List[B], cs: List[C]): List[C] =
      (as, bs) match {
        case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(f(x, y), cs))
        case _ => cs
      }
    reverse(go(as, bs, Nil))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith(as: List[A], prefix: List[A]): Boolean =
      (as, prefix) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x == y)
            startsWith(xs, ys)
          else
            false
      }
    sup match {
      case Nil => false
      case Cons(_, xs) =>
        if (startsWith(sup, sub))
          true
        else
          hasSubsequence(xs, sub)
    }
  }

}

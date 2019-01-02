package extensible

sealed trait Union

sealed trait Void extends Union

sealed trait :+:[F[_], U] extends Union

case class Inl[F[_], A, U](fa: F[A]) extends (F :+: U)

case class Inr[F[_], U](u: U) extends (F :+: U)

trait Member[F[_], U] {
  def inject[A](f: F[A]): U
}

object Member {
  def apply[F[_], U](implicit member: Member[F, U]): Member[F, U] = member

  implicit def left[F[_], U]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](f: F[A]): F :+: U = Inl(f)
    }

  implicit def right[F[_], G[_], U](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](f: F[A]): G :+: U = Inr(member.inject(f))
    }
}

sealed trait Eff[U, A] {
  def map[B](f: A => B): Eff[U, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Eff[U, B]): Eff[U, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(u, g) => Impure(u, g :+ f)
    }
}

case class Pure[U, A](a: A) extends Eff[U, A]

case class Impure[U, A, B](u: U, f: Arrows[U, A, B]) extends Eff[U, B]

object Eff {
  def apply[U, F[_], A](fa: F[Eff[U, A]])(implicit F: Member[F, U]): Eff[U, A] = Impure(F.inject(fa), Leaf((x: Eff[U, A]) => x))

  def run[A](eff: Eff[Void, A]): A =
    eff match {
      case Pure(a) => a
    }
}

sealed trait Arrows[U, A, B] {
  def apply(a: A): Eff[U, B] = {
    @scala.annotation.tailrec
    def go[A](f: Arrows[U, A, B], a: A): Eff[U, B] =
      f.view match {
        case One(f) => f(a)
        case Cons(f, r) =>
          f(a) match {
            case Pure(v) => go(r, v)
            case Impure(f, l) => Impure(f, l ++ r)
          }
      }
    go(this, a)
  }

  def :+[C](f: B => Eff[U, C]): Arrows[U, A, C] = Node(this, Leaf(f))

  def ++[C](q: Arrows[U, B, C]): Arrows[U, A, C] = Node(this, q)

  def view: View[U, A, B] =
    this match {
      case Leaf(f) => One(f)
      case Node(l, r) =>
        @scala.annotation.tailrec
        def go[T](x: Arrows[U, A, T], y: Arrows[U, T, B]): View[U, A, B] =
          x match {
            case Leaf(f) => Cons(f, y)
            case Node(l, r) => go(l, Node(r, y))
          }
        go(l, r)
    }
}

case class Leaf[U, A, B](f: A => Eff[U, B]) extends Arrows[U, A, B]

case class Node[U, A, B, C](left: Arrows[U, A, B], right: Arrows[U, B, C]) extends Arrows[U, A, C]

sealed trait View[U, A, B]

case class One[U, A, B](f: A => Eff[U, B]) extends View[U, A, B]

case class Cons[U, A, B, C](f: A => Eff[U, B], k: Arrows[U, B, C]) extends View[U, A, C]

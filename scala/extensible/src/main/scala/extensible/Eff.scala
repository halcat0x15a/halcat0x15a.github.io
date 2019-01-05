package extensible

sealed trait Union[F[_], G[_], A]

case class Inl[F[_], G[_], A](value: F[A]) extends Union[F, G, A]

case class Inr[F[_], G[_], A](value: G[A]) extends Union[F, G, A]

sealed trait Void[A]

trait Member[F[_], G[_]] {
  def inject[A](f: F[A]): G[A]
}

object Member {
  def apply[F[_], G[_]](implicit member: Member[F, G]): Member[F, G] = member

  implicit def left[F[_], G[_]]: Member[F, (F :+: G)#R] =
    new Member[F, (F :+: G)#R] {
      def inject[A](fa: F[A]): (F :+: G)#R[A] = Inl(fa)
    }

  implicit def right[F[_], G[_], H[_]](implicit member: Member[F, H]): Member[F, (G :+: H)#R] =
    new Member[F, (G :+: H)#R] {
      def inject[A](fa: F[A]): (G :+: H)#R[A] = Inr(member.inject(fa))
    }
}

sealed trait Eff[R[_], A] {
  def map[B](f: A => B): Eff[R, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(r, k) => Impure(r, k :+ f)
    }
}

case class Pure[R[_], A](a: A) extends Eff[R, A]

case class Impure[R[_], A, B](union: R[A], k: Arrows[R, A, B]) extends Eff[R, B]

object Eff {
  def apply[R[_], F[_], A](fa: F[A])(implicit F: Member[F, R]): Eff[R, A] =
    Impure(F.inject(fa), Leaf((x: A) => Pure(x)))

  def run[A](eff: Eff[Void, A]): A =
    eff match {
      case Pure(a) => a
    }
}

sealed trait Arrows[R[_], A, B] {
  def apply(a: A): Eff[R, B] = {
    @scala.annotation.tailrec
    def go[A](f: Arrows[R, A, B], a: A): Eff[R, B] =
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

  def :+[C](f: B => Eff[R, C]): Arrows[R, A, C] = Node(this, Leaf(f))

  def ++[C](q: Arrows[R, B, C]): Arrows[R, A, C] = Node(this, q)

  def view: View[R, A, B] =
    this match {
      case Leaf(f) => One(f)
      case Node(l, r) =>
        @scala.annotation.tailrec
        def go[T](x: Arrows[R, A, T], y: Arrows[R, T, B]): View[R, A, B] =
          x match {
            case Leaf(f) => Cons(f, y)
            case Node(l, r) => go(l, Node(r, y))
          }
        go(l, r)
    }
}

case class Leaf[R[_], A, B](f: A => Eff[R, B]) extends Arrows[R, A, B]

case class Node[R[_], A, B, C](left: Arrows[R, A, B], right: Arrows[R, B, C]) extends Arrows[R, A, C]

sealed trait View[R[_], A, B]

case class One[R[_], A, B](f: A => Eff[R, B]) extends View[R, A, B]

case class Cons[R[_], A, B, C](f: A => Eff[R, B], k: Arrows[R, B, C]) extends View[R, A, C]

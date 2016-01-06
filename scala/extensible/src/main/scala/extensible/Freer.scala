package extensible

sealed trait Union

sealed trait Void extends Union

sealed trait :+:[F[_], U <: Union] extends Union

case class Inl[F[_], A, U <: Union](fa: F[A]) extends (F :+: U)

case class Inr[F[_], U <: Union](u: U) extends (F :+: U)

trait Member[F[_], U <: Union] {

  def inject[A](f: F[A]): U

}

object Member {

  def apply[F[_], U <: Union](implicit member: Member[F, U]): Member[F, U] = member

  implicit def left[F[_], U <: Union]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](f: F[A]): F :+: U = Inl(f)
    }

  implicit def right[F[_], G[_], U <: Union](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](f: F[A]): G :+: U = Inr(member.inject(f))
    }

}

sealed trait Freer[U <: Union, A] {

  def map[B](f: A => B): Freer[U, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Freer[U, B]): Freer[U, B] =
    this match {
      case Pure(a) => f(a)
      case Impure(u, g) => Impure(u, g :+ f)
    }

}

case class Pure[U <: Union, A](a: A) extends Freer[U, A]

case class Impure[U <: Union, A, B](u: U, f: Arrows[U, A, B]) extends Freer[U, B]

object Freer {

  def apply[U <: Union, F[_], A](fa: F[Freer[U, A]])(implicit F: Member[F, U]): Freer[U, A] = Impure(F.inject(fa), Leaf((x: Freer[U, A]) => x))

  def run[A](freer: Freer[Void, A]): A =
    freer match {
      case Pure(a) => a
    }

}

sealed trait Arrows[U <: Union, A, B] {

  def apply(a: A): Freer[U, B] = {
    @scala.annotation.tailrec
    def go(f: Arrows[U, Any, B], a: Any): Freer[U, B] =
      f.view match {
        case One(f) => f(a)
        case Cons(f, r) =>
          f(a) match {
            case Pure(v) => go(r, v)
            case Impure(f, l) => Impure(f, l ++ r)
          }
      }
    go(this.asInstanceOf[Arrows[U, Any, B]], a)
  }

  def :+[C](f: B => Freer[U, C]): Arrows[U, A, C] = Node(this, Leaf(f))

  def ++[C](q: Arrows[U, B, C]): Arrows[U, A, C] = Node(this, q)

  def view: View[U, A, B] =
    this match {
      case Leaf(f) => One(f)
      case Node(l, r) =>
        @scala.annotation.tailrec
        def go(x: Arrows[U, A, Any], y: Arrows[U, Any, B]): View[U, A, B] =
          x match {
            case Leaf(f) => Cons(f, y)
            case Node(l, r) => go(l, Node(r, y))
          }
        go(l, r)
    }

}

case class Leaf[U <: Union, A, B](f: A => Freer[U, B]) extends Arrows[U, A, B]

case class Node[U <: Union, A, B, C](left: Arrows[U, A, B], right: Arrows[U, B, C]) extends Arrows[U, A, C]

sealed trait View[U <: Union, A, B]

case class One[U <: Union, A, B](f: A => Freer[U, B]) extends View[U, A, B]

case class Cons[U <: Union, A, B, C](f: A => Freer[U, B], k: Arrows[U, B, C]) extends View[U, A, C]

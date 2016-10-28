import scala.language.{higherKinds, existentials}

package object curryhoward {

  type Not[A] = A => Nothing

  type Or[A, B] = Not[Not[A] with Not[B]]

  def double[A](a: A)(implicit ev: Not[Not[A]] <:< Or[Int, String]): String =
    a match {
      case i: Int => (i + i).toString
      case s: String => s + s
    }

  type Nat[F[_], G[_]] = Forall[({ type H[A] = F[A] => G[A] })#H]

  /*
  trait Forall[P[_]] {
    def apply[A]: P[A]
  }

  def mapping[F[_], G[_], A, B](f: Nat[F, G])(pair: (F[A], F[B])): (G[A], G[B]) = (f[A](pair._1), f[B](pair._2))

  def opt2list: Nat[Option, List] =
    new Nat[Option, List] {
      def apply[A]: Option[A] => List[A] = _.toList
    }

  def list2opt: Nat[List, Option] =
    new Nat[List, Option] {
      def apply[A]: List[A] => Option[A] = _.headOption
    }
   */

  type Forall[P[_]] = Not[Not[P[A]] forSome { type A }]

  def callCC[A](ev: Not[Not[A]]): A = ev(a => return a)

  def mapping[F[_], G[_], A, B](f: Nat[F, G])(pair: (F[A], F[B])): (G[A], G[B]) =
    (callCC[F[A] => G[A]](f)(pair._1), callCC[F[B] => G[B]](f)(pair._2))

  def opt2list: Nat[Option, List] = k => k(_.toList)

  def list2opt: Nat[List, Option] = k => k(_.headOption)

  type Foo = { def foo: String; def baz: Int }
  type Bar = { def bar: String; def baz: Int }

  implicitly[Foo with Bar <:< { def foo: String; def bar: String; def baz: Int }]
  implicitly[Or[Foo, Bar] <:< Not[Not[{ def baz: Int }]]]

}

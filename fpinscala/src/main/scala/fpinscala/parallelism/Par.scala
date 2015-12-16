package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Executors, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      lazy val fa = a(es)
      lazy val fb = b(es)
      new Future[C] {
        def isDone = fa.isDone && fb.isDone
        def get: C = f(fa.get, fb.get)
        def get(timeout: Long, unit: TimeUnit): C = {
          val start = System.nanoTime
          val a = fa.get(timeout, unit)
          val elapsed = System.nanoTime - start
          f(a, fb.get(timeout - elapsed, unit))
        }
        def isCancelled = fa.isCancelled && fb.isCancelled
        def cancel(evenIfRunning: Boolean): Boolean = fa.cancel(evenIfRunning) && fb.cancel(evenIfRunning)
      }
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      map(sequence(as.map(asyncF((a: A) => if (f(a)) List(a) else Nil))))(_.flatten)
    }

  def lazyUnit[A](a: => A): Par[A] = ???

  def run[A](e: ExecutorService)(a: Par[A]): Future[A] = a(e)

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n).get)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(key).get)(es)

  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => f(run(es)(pa).get)(es)

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

  def join2[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a).get()(es)

}

/*
map(y)(id) == y

map(map(y)(g))(f) == map(y)(f compose g)

// map(map(y)(id))(f) == map(y)(f compose id)
// map(y)(f) == map(y)(f)

y: F[A]

g: A => B

f: B => C

map(map(y)(g))(f): F[C]

 map(y)(f compose g): F[C]


 */

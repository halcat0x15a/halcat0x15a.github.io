package object stackless {

  type Trampoline[+A] = Free[Function0, A]

  type Pair[+A] = (A, A)

  type BinTree[+A] = Free[Pair, A]

  type FreeState[S, +A] = Free[({ type F[+B] = StateF[S, B] })#F, A]

  def foldl[A, B](as: List[A], b: B, f: (B, A) => B): B =
    as match {
      case Nil => b
      case x :: xs => foldl(xs, f(b, x), f)
    }

  def pureState[S, A](a: A): FreeState[S, A] =
    Done[({ type F[+B] = StateF[S, B] })#F, A](a)

  def getState[S]: FreeState[S, S] =
    More[({ type F[+B] = StateF[S, B] })#F, S](Get(s => pureState(s)))

  def setState[S](s: S): FreeState[S, Unit] =
    More[({ type F[+B] = StateF[S, B] })#F, Unit](Put(s, pureState(())))

  def evalS[S, A](s: S, t: FreeState[S, A]): A =
    t.resume match {
      case Left(Get(f)) => evalS(s, f(s))
      case Left(Put(n, a)) => evalS(n, a)
      case Right(a) => a
    }

  def factorial(n: BigInt): Trampoline[BigInt] =
    if (n <= 1)
      Done(1)
    else
      More(() => factorial(n - 1).map(n * _))

  def fib(n: Int): Trampoline[Int] =
    if (n < 2)
      Done(n)
    else
      for {
        x <- More(() => fib(n - 1))
        y <- More(() => fib(n - 2))
      } yield x + y

  def even(n: Int): Trampoline[Boolean] =
    if (n <= 0)
      Done(true)
    else
      More(() => odd(n - 1))

  def odd(n: Int): Trampoline[Boolean] =
    if (n <= 0)
      Done(false)
    else
      More(() => even(n - 1))

}

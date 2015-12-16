package fpinscala.gettingstarted

object MyModule {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, x: Int, y: Int): Int =
      if (n <= 0)
        x
      else
        go(n - 1, y, x + y)
    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n <= 0)
        true
      else if (ordered(as(n - 1), as(n)))
        go(n - 1)
      else
        false
    go(as.length - 1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}

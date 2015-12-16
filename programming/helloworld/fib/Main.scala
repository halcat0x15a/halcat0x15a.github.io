object Main extends App {
  val fib: Stream[Long] = 0L #:: fib.scanLeft(1L)(_ + _)
  fib.take(50).foreach(println)
}

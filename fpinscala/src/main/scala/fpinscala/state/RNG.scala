package fpinscala.state

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt
    if (n < 0)
      (-(n + 1), next)
    else
      (n, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, next) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, next) = rng.nextInt
    double(next) match {
      case (d, next) => ((i, d), next)
    }
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, next) = double(rng)
    next.nextInt match {
      case (i, next) => ((d, i), next)
    }
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, next) = double(rng)
    double(next) match {
      case (d2, next) =>
        double(next) match {
          case (d3, next) => ((d1, d2, d3), next)
        }
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(acc: List[Int], count: Int, rng: RNG): (List[Int], RNG) =
      if (count <= 0)
        (acc, rng)
      else
        rng.nextInt match {
          case (n, next) => go(n :: acc, count - 1, next)
        }
    go(Nil, count, rng)
  }

}

object Rand {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = { rng => (a, rng) }

  /*
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }
   */

  val double: Rand[Double] =
    map(RNG.nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  /*
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }
   */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((s, acc) => map2(s, acc)(_ :: _))

  def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(RNG.nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

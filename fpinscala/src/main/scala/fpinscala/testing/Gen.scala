package fpinscala.testing

import java.util.concurrent.Executors

import fpinscala.state.{Rand, RNG, State}
import fpinscala.laziness.Stream
import fpinscala.parallelism.Par, Par._

case class SGen[A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(size => forSize(size).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(size => forSize(size).flatMap(a => f(a).forSize(size)))

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(Gen.listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(if (n > 0) n else 1, g))

}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    flatMap(a => g.map(b => f(a, b)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  def choose(start: Int, stop: Int): Gen[Int] = {
    @annotation.tailrec
    def go(rng: RNG): (Int, RNG) = {
      val r@(n, rng2) = rng.nextInt
      if (start <= n && n < stop)
        r
      else
        go(rng2)
    }
    Gen(State(go))
  }

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(Rand.int).map(_ >= 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    g1.flatMap(x => g2.flatMap(y => boolean.flatMap(if (_) unit(x) else unit(y))))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (a1, x) = g1
    val (a2, y) = g2
    Gen(State(Rand.double)).flatMap(d => if (x / (x + y) > d) a1 else a2)
  }

  type TestCases = Int

  type FailedCase = String
  
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  type MaxSize = Int

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    def &&(that: => Prop): Prop =
      Prop { (max, cases, rng) =>
        run(max, cases, rng) match {
          case Passed => that.run(max, cases, rng)
          case falsified => falsified
        }
      }

    def ||(that: => Prop): Prop =
      Prop { (max, cases, rng) =>
        run(max, cases, rng) match {
          case Passed => Passed
          case _ => that.run(max, cases, rng)
        }
      }

  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
        case (a, i) =>
          try {
            if (f(a))
              Passed
            else
              Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }.find(_.isFalsified).getOrElse(Passed)
    }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    val x: scala.collection.immutable.Stream[A] =
      scala.collection.immutable.Stream.iterate((Option.empty[A], rng)) {
        case (opt, rng) => g.sample.run(rng) match { case (a, rng) => Some(a) -> rng }
      }.flatMap {
        case (opt, _) => opt
      }
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       | ${e.getStackTrace.mkString("\n")}""".stripMargin

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop = props.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }

  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    ns.forall(_ <= max)
  }

  val sortProp = forAll(SGen.listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.isEmpty || sorted.zip(sorted.tail).forall { case (x, y) => x <= y }
  }

  val pint = Gen.choose(-10, 10).map(Par.unit(_))

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  val forkProp = forAllPar(pint)(n => equal(Par.fork(n), n))

  val isEven = (_: Int) % 2 == 0
  val takeWhileProp = forAll(SGen.listOf(smallInt))(ns => ns.takeWhile(isEven).forall(isEven) && ns.takeWhile(isEven) ::: ns.dropWhile(isEven) == ns)

}

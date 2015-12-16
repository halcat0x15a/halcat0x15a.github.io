package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, CountDownLatch}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  type Par[A] = ExecutorService => Future[A]

  trait Future[A] {
    def apply(k: A => Unit): Unit
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown
    }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

}

package chapter_7

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

sealed trait Par[A]

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = ???

  def lazyAnit[A](a: A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = ???

  private case class UnitFuture[A](get: A) extends Future[A]
 {
   def isDone = true
   def get(timeout: Long, units: TimeUnit) = get
   def isCancelled = false
   def cancel(evenIfRunning: Boolean): Boolean = false
 }
  /**
    *
    * exercise 7.1
    * @param a
    * @param b
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2Def[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] ={
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(),bf.get()))
    }
  }



}

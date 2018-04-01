package chapter_7

sealed trait Par[A]

object Par {

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
    ???
  }

}

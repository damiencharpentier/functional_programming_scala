package main.chapter_2

/**
  * Created by damiencharpentier on 17-11-02.
  */
object Curried {

  def curried1[A,B,C](f: (A,B) => C): A => (B => C) =
      a => b => f(a,b)

}

package main.chapter_2

/**
  * Created by damiencharpentier on 17-11-06.
  */
object Compose {

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a));
}

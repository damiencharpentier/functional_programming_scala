package chapter_5

/**
  * Created by damiencharpentier on 18-02-20
  */
sealed trait Stream[+A] {

  /**
    * This method transforms the stream into a list
    *
    * @return list constructed from stream
    */
  def toList(): List[A] = {
     this match {
        case Empty => List()
        case Cons(hd, tl) => hd() :: tl().toList()
      }
  }

  /**
    * Returns a Stream of the first n elements given as parameter
    * or the entire Stream if n > Stream size
    * @param n
    * @return
    */
  def take(n: Int): Stream[A] = {
    if(n == 0){
      return Stream.empty
    } else {
      this match {
          case Empty => Stream.empty
          case Cons(hd, tl) => Stream.cons(hd(), tl().take(n-1))
      }
    }
  }

  /**
    * Drops from the stream the n number of elements given as parameter
    * or an empty stream if n > Stream size
    * or the entire Stream if n > Stream size
    * @param n
    * @return
    */
  def drop(n: Int): Stream[A] = {
    if(n == 0) {
      return this
    } else {
      this match {
          case Empty => Stream.empty
          case Cons(hd, tl) => tl().drop(n-1)
      }
    }
  }

  /**
    * Takes elements from the Stream as long as each element satisfies the predicate
    * @param p
    * @return
    */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
        case Empty => Stream.empty
        case Cons(hd, tl) => {
          if(!p(hd())) {
            Stream.empty
          } else {
            Stream.cons(hd(), tl().takeWhile(p))
          }
        }
    }
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  /**
    * "Smart" constructor that takes an element and a Stream as parameter and make them lazy
    * before calling the real constructor.
    * It enables lazy evaluation
    * @param hd
    * @param tl
    * @tparam A
    * @return
    */
  def cons[A](hd: => A, tl: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}

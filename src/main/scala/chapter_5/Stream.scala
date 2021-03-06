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

  /**
    * Exercise 5.4
    * evaluate predicate for each element of the stream.
    * Stops when first predicate evaluation returns false or at the end of the list
    * @param p
    * @return
    */
  def forAll(p: A => Boolean): Boolean = {
    this match {
        case Empty => true
        case Cons(hd, tl) => p(hd()) && tl().forAll(p)
    }
  }

  /**
    * FoldRight on Stream
    * @param default
    * @param f
    * @tparam B
    * @return
    */
  def foldRight[B](default: Stream[B])(f: (A, Stream[B]) => Stream[B]) : Stream[B] = {

    this match{
      case Cons(h,t) => f(h(),t().foldRight(default)(f))
      case _ => default
    }

  }

  /**
    * Exercise 5.5
    * use foldRight to implement takeWhile
    */
  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty)((elem: A, tail: Stream[A]) => if(!p(elem)){ tail } else { Stream.cons(elem,tail) })
  }

  /**
    * Exercise 5.7
    * map method
    */
  def map[B](f: A => B): Stream[B] = {
    this.foldRight(Stream.empty: Stream[B])((elem, tail) => Stream.cons(f(elem),tail))
  }

  /**
    * Exercise 5.7
    * filter method
    */
  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty: Stream[A])((elem, tail)=> if(!f(elem)) tail else Stream.cons(elem,tail))
  }

  /**
    * Exercise 5.7
    * append another stream to this one
    *
    */
  def append[B >: A](other: Stream[B]): Stream[B] = {
    this.foldRight(other)((elem, tail) => Stream.cons(elem, tail))
  }

  /**
    * Exercise 5.7
    *
    * flatMap using foldRight
    *
    */
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight[B](Stream.empty : Stream[B])((elem, tail)=> f(elem).append(tail))
  }


  def forEach(f: A => Unit): Unit = {
    this match {
        case Cons(head,_) => f(head())
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

  /**
    * Returns an infinite Stream of the given value
    * (From book answers)
    * @param a
    * @tparam A
    * @return
    */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /**
    * Exercise 5.9
    * @param n
    * @return
    */
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  /**
    * Exercise 5.10
    * Use infinite Stream to generate Fibonacci number
    * TODO does not seem to work...
    */
  def fibs: Stream[Int] = {

    def go (first: Int, second: Int): Stream[Int] = {
      go(second, first + second)
    }

    Stream.cons(0,cons(1, go(1,1)))
  }

}

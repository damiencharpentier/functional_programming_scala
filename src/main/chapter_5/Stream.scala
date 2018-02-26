package chapter_5

/**
  * Created by damiencharpentier on 18-02-20.
  */
sealed trait Stream[+A] {

  /**
    * this method transforms the stream into a list
    *
    * @return list constructed from stream
    */
  def toList(): List[A] = {
     this match {
        case Empty => List()
        case Cons(hd: A, tl: Stream[A]) => hd :: tl.toList()
      }
  }


  def take(n: Int): Stream[A] = {
    if(n == 0){
      return this
    } else {
      this match {
          case Empty => Stream.empty
          case Cons(hd: A, tl: Stream[A]) => Stream.cons(hd, tl.take(n-1))
      }
    }
  }

  def drop(n: Int): Stream[A] = {
    if(n == 0) {
      return this
    } else {
      this match {
          case Empty => Stream.empty
          case Cons(hd: A, tl: Stream[A]) => tl.drop(n-1)
      }
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
        case Empty => Stream.empty
        case Cons(hd :A, tl: Stream[A]) => {
          if(!p(tl)) {
            Stream.cons(hd, Stream.empty)
          } else {
            Stream.cons(hd, tl.takeWhile(p))
          }
        }
    }
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

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

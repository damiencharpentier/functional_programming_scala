package main.chapter_4

/**
  * Created by damiencharpentier on 17-11-27.
  */

sealed trait Option[+A]{


  /**
    * exercise 4.1
    * @param f
    * @tparam B
    * @return
    */
  def map[B](f: A => B): Option[B] = {
    this match {
       case None => None
       case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {

    this match {
       case None => None
       case Some(a) => f(a)
    }
  }

  def getOrElse[B >: A](default: => B): B = {

    this match {
      case None => default
      case Some(a) => a
    }

  }

  def orElse[B >: A](ob: => Option[B]): Option[B] ={

    this match {
        case None => ob
        case Some(a) => Some(a)
    }

  }


  /**
    * filter: if this does not have any value, return Non e
    * If this has value but does not satisfy the test, return None
    * Otherwise return this.
    * @param f
    * @return
    */
  def filter(f: A => Boolean): Option[A] = {

    this match {
        case None => None
        case Some(a) => {
          if(f(a)){
            this
          } else {
            None
          }
        }
    }

  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {

    if(xs.isEmpty){
      return None;
    } else {
      Some(xs.sum / xs.length)
    }
  }


  /**
    * took from answer
    * @param xs
    * @return
    */
  def variance (xs: Seq[Double]) : Option[Double] = {

    val meanResult = mean(xs)

    meanResult flatMap(m => mean(xs.map(elem => math.pow(elem - m, 2))))

  }


  /**
    * for reference from book:
    * function that transforms a function A=>B into a function Option[A]=>Option B
    */
  def lift[A,B](f: A => B): Option[A] => Option[B] = {
    _ map f
  }


  /**
    * exercise 4.3
    * define map2 which return None if either one of the two parameters is not defined
    * otherwise apply function on them
    */
  def map2[A,B,C](a: Option[A], b:Option[B])(f: (A,B) => C): Option[C] = {
    a flatMap(a => b map(b => f(a,b)))
  }

  /**
    * exercise 4.4
    * sequence function that combine list of option into one option of list.
    * returns None if one of the element is None
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List()))((elem: Option[A],acc: Option[List[A]]) => acc match {
      case Some(Nil) => Some(Nil)
      case Some(list) => {
        elem match {
          case None => Some(Nil)
          case Some(e) => {
            Some(e :: list)
          }
        }
      }
    })
  }

  /**
    * Exercise 4.5
    *
    * function that interates on list of type A, and transforms it in some list of B using
    * mapping function or None if of the transformations fails
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List[B]()))((elem: A, acc: Option[List[B]]) =>  {
      map2(f(elem), acc)((mapped,list) => mapped :: list)
    })
  }

  /**
    * Exercise 4.5
    * write sequence in terms of traverse
    */
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)((elem : Option[A]) => elem)
  }


}



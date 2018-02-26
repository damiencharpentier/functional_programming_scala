package chapter_4

import main.chapter_4.{None, Option, Some}

sealed trait Either[+E, +A] {

  /**
    * Exercise 4.6
    *
    * @param f
    * @tparam B
    * @return
    */
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(elem) => Right(f(elem))
      case Left(elem) => Left(elem)
    }
  }

  /**
    * Exercise 4.6
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(elem) => f(elem)
      case Left(elem) => Left(elem)
    }
  }

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(elem) => Right(elem)
      case Left(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this flatMap (a => b map (b => f(a, b)))
  }


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either {

  /**
    * exercise 4.7
    * iterates list of either results or single element containing exception
    * and tranforms it into a Either object being either the exception or the list of results
    * @param es
    * @tparam E
    * @tparam A
    * @return
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(List()))((elem: Either[E, A], acc: Either[E, List[A]]) => {
      acc match {
        case Left(ex) => Left(ex)
        case Right(list) => {
          elem match {
            case Left(ex) => Left(ex)
            case Right(elem) => Right(elem :: list)
          }
        }
      }
    })
  }


  /**
    * traverses a list of elements of type A
    * if an exception has been encountered, returns it and don't try further transformations
    * otherwise transformation from A to Either[E,B] is applied.
    * If result is a exception, returns its details
    * otherwise appends the transformed result to the list of transformed values
    * @param as
    * @param f
    * @tparam E
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E,List[B]]](Right(List()))((elem: A, list: Either[E,List[B]]) => {
      list match {
          case Left(ex) => Left(ex)
          case Right(list) => {
            f(elem) match {
                case Left(ex) => Left(ex)
                case Right(result) => Right(result :: list)
            }
          }
      }
    })
  }
}
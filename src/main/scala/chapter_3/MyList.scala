package chapter_3

import scala.annotation.tailrec


/**
  * Exercises of chapter 3
  *
  * Created by damiencharpentier on 17-11-07.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  /**
    * append
    */
  def append[A](list1: List[A], list2: List[A]): List[A] =
    list1 match {
      case Nil => list2
      case Cons(h,t) => Cons(h, append(t, list2))
    }


  /**
    * drops the first element of the list and returns the rest
    * @param x list to drop the first element from
    * @tparam A the type of the list
    * @return the list without its first element
    */
  def tail[A](x: List[A]): List[A] = {

    x match {
      case Nil => Nil;
      case Cons(s, Nil) => Nil
      case Cons(s, xs: Cons[A]) => xs

    }

  }

  /**
    * replaces the first element of a list with an element given in parameter
    * @param newHead the new first element
    * @param x list to replace the head
    * @tparam A type of the list
    * @return a list with the first element replaced
    */
  def setHead[A](newHead: A, x: List[A]): List[A] = {

    Cons(newHead, tail(x))

  }

  /**
    *
    * @param x
    * @param n
    * @tparam A
    * @return
    */
  def drop[A](x: List[A], n: Int): List[A] = {

    if(n <= 0){
      return x
    }

    x match {
        case Nil => Nil;
        case Cons(h, t) => drop(t, n-1)
    }

  }

  def dropWhile[A](x: List[A], f: A => Boolean): List[A] = {

    x match {
        case Nil => Nil;
        case Cons(h, t) => {
          if(!f(h)){
            return x
          }
          dropWhile(t,f);
        }
    }

  }


  def mainDropWhile (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Nil))))
    def test(x: String) : Boolean = if(x.length<5){ return true} else { return false};
    println(dropWhile[String](myList, test))

  }

  def init[A](l: List[A]): List[A] = {

    l match {
      case Nil => Nil
      case Cons(h,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))

    }

  }


  def mainInit (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Nil))))
    println(init[String](myList))


  }

  /**
    * exercise 3.7a
    *
    * @param l
    * @param default
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A,B](l: List[A],default: B)(f: (A,B) => B) : B ={

    l match{
        case Nil => default
        case Cons(h,t) => f(h,foldRight(t,default)(f))

    }
  }

  /**
    * exercice 3.8
    * answer:
    * when passing Nil and Cons to foldRight function
    * a linked list is built (the same as the one passed as argument
    * The linked list is built while going back higher in the stack once Nil is found
    */


  /**
    * exercice 3.9
    * calculate the length of a list using foldRight
    * when we encounter Nil, we return zero and we add up 1 has many time
    * as we call the sum function
    */
  def length[A](list: List[A]): Int = {

    foldRight(list, 0)((x,y) => 1+y)

  }


  def main (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))
    println(length[String](myList))// should be 5
    println(length[String](Cons("popo",Nil))) // should be 1
    println(length[String](Nil)) // should be zero
  }

  /**
    * exercice 3.10
    * writing foldLeft function that makes tail recursion possible
    * for that, we need apply the method f  to the arguments default and the head before calling
    * again foldLeft. In that case, the new intermediary value is passed to foldLeft, optimizing stack usage
    */
  @tailrec
  def foldLeft[A,B](list: List[A], default: B)(f: (B, A) => B) : B = {

    list match{
      case Nil => default
      case Cons(h,t) => foldLeft(t,f(default, h))(f)

    }
  }

  /**
    * exercice 3.10 testing length with foldLeft
    * calculate the length of a list using foldLeft
    * Here in the function, we ignore argument of type A, we just want to add 1 to the accumulator x
    */
  def lengthFoldLeft[A](list: List[A]): Int = {

    foldLeft(list, 0)((x,y) => x + 1)

  }

  /**
    * exercice 3.10  main method for foldLeft tests
    */
  def mainFoldLeft (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))

    println(foldLeft[String, Int](myList,0)((x,A) => x + 1))// should be 5

    println(lengthFoldLeft[String](Cons("popo",Nil))) // should be 1

    println(lengthFoldLeft[String](Nil)) // should be zero

    // now try with multiplication using foldLeft
    val myListInt = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))) // when multiplied, should be equal to 120

    println(foldLeft[Int, Int](myListInt, 1)((x,y) => x * y)) // should be 120

  }




  /**
    * exercice 3.11  main method for foldLeft tests
    */
  def mainFoldLeftMultiplicationSumLength (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))

    println(foldLeft[String, Int](myList,0)((x,A) => x + 1))// should be 5

    println(foldLeft[String, Int](Cons("popo",Nil),0)((x,A) => x + 1)) // should be 1

    println(foldLeft[String, Int](Nil, 0)((x,A) => x + 1)) // should be zero

    // now try with multiplication using foldLeft
    val myListInt = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))) // when multiplied, should be equal to 120

    println(foldLeft[Int, Int](myListInt, 1)((x,y) => x * y)) // should be 120

    println(foldLeft[Int, Int](myListInt, 0)((x,y) => x + y)) // should be 15


  }


  /**
    * exercice 3.12  main method for reversing list with fold functions
    */
  def mainReverse (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))

    //using fold left
    println(foldLeft[String, List[String]](myList,Nil)((x,y) => Cons(y,x)))

    //using fold right, I cannot get it work, is not reversed
    println(foldRight[String, List[String]](myList,Nil)((x,y) => Cons(x,y)))


  }


  /**
    * exercice 3.13  writing fold left in terms of fold right
    */



  /**
    * exercice 3.13  writing fold left in terms of fold right
    */
  def mainFoldLeftFoldRight (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))

    //TODO

  }


  /**
    * exercice 3.14  append two lists using fold left and right
    */

  def mainAppend (args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))

    val myList2 = Cons[String]("hoho", Cons("haha", Cons("hihi", Cons("huhu", Cons("houhou", Nil)))))

    //using fold left
    println(foldLeft(myList,myList2)((x,y) => Cons(y,x)))

    //using fold right
    println(foldRight(myList,myList2)((x,y) => Cons(x,y)))
  }



  /**
    * exercice 3.15  TODO
    */



  /**
    * exercice 3.16  transform a list of integers by adding 1 to each element
    * return a new list
    */
  def mainAdd (args : Array[String]): Unit ={
    val myList = Cons(1, Cons(2, Cons(4, Cons(8, Cons(16, Nil)))))


    println(foldRight(myList, Nil:List[Int])((x,y:List[Int]) => Cons(x+1,y)))
    println(foldLeft(myList, Nil:List[Int])((x:List[Int],y) => Cons(y+1,x)))
  }

  /**
    * exercice 3.17  transforms each element of List[Double] into a String
    */
  def mainTransformListOfDoubleToString (args : Array[String]): Unit ={
    val myList = Cons(1.0, Cons(2.0, Cons(4.0, Cons(8.0, Cons(16.0, Nil)))))


    println(foldRight[Double,List[String]](myList: List[Double], Nil:List[String])((x,y:List[String]) => Cons(x.toString,y)))
  }

  /**
    * exercice 3.18  write map function to generalize transformation
    */
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight[A,List[B]](as, Nil:List[B])((x,y)=> Cons(f(x),y))
  }

  def mapFoldLeft[A,B](as: List[A])(f: A => B): List[B] = {
    foldLeft[A,List[B]](as, Nil:List[B])((x,y)=> Cons(f(y),x))
  }

  //example using map
  def mainMap(args : Array[String]): Unit ={
    val myList = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))

    // return a list of the strings but reversed...
    println(map(myList)(x => x.reverse))
  }

  /**
    * exercice 3.19 filter method to remove element if satisfies predicate
    *
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight[A,List[A]](as, Nil:List[A])((x,y)=> if(f(x)) y else Cons(x,y))
  }


  /**
    * exercice 3.19  write filter using fold
    */

  //example using filter
  def mainFilter(args : Array[String]): Unit ={
    val myList = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));
    def detectOdd( x: Int ): Boolean = if(x%2==0) false else true;
    // return a list of the strings but reversed...
    println(filter(myList)(detectOdd))
  }


  /**
    * Exercice 3.20 flatmap function
    */

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight[A, List[B]](as, Nil:List[B])((x,y)=> append(f(x),y))
  }

  def flatMapLeft[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft[A, List[B]](as, Nil:List[B])((x,y)=> append(f(y),x))
  }

  def mainFlatMap(args : Array[String]): Unit ={
    val myList = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));

    def double(i:Int) : List[Int] = Cons(i,Cons(i,Nil));

    println(flatMap(myList)(double))
  }

  /**
    * exercice 3.21 use flat map to implement filter
   */
  def mainFlatMapFilter(args : Array[String]): Unit ={
    val myList = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));

    def detectOdd( x: Int ): Boolean = if(x%2==0) false else true;

    println(flatMap(myList)( i => if(detectOdd(i)) Nil else Cons(i,Nil)));
  }

  /**
    * exercice 3.22 combine elements of two list into one list
    * List(1,2,3) and List(4,5,6) => List (5,7,9)
    */
  def combine(list1:List[ Int],list2:List[Int]): List[Int] = {
    (list1,list2) match {
        case (list1, Nil) => Nil;
        case (Nil, list1) => Nil;
        case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,combine(t1,t2))

    }
  }

  /**
    * exercice 3.21 test combine
    */
  def mainCombine(args : Array[String]): Unit ={
    val myList1 = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));
    val myList2 = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));

    println(combine(myList1,myList2));
  }

  /**
    * exercice 3.23 combine elements of two list into one list
    * List(1,2,3) and List(4,5,6) => List (5,7,9)
    */
  def zipWith[A](list1:List[A],list2:List[A])(f: (A,A) => A ): List[A] = {
    (list1,list2) match {

      case (list1, Nil) => Nil;
      case (Nil, list1) => Nil;
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))

    }
  }

  /**
    * exercice 3.23 generalize zipWith
    */
  def mains(args : Array[String]): Unit ={
    val myList1 = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));
    val myList2 = Cons[Int](1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));

    def combineInt(x: Int, y:Int): Int = x + y ;

    println(zipWith(myList1,myList2)(combineInt));


    val myListStr1 = Cons[String]("caca", Cons("popo", Cons("pipipi", Cons("pupu", Cons("prout", Nil)))))
    val myListStr2 = Cons[String]("haha", Cons("hoho", Cons("hihi", Cons("huhu", Cons("houhou", Nil)))))

    def combineString(x: String, y:String): String = x.concat(y) ;

    println(zipWith(myListStr1,myListStr2)(combineString));

  }


}
package fibonacci.sorted

import scala.annotation.tailrec

/**
  * Created by damiencharpentier on 17-11-01.
  */
object Sorted {

  def isSorted[A](arr: Array[A], sorting: (A,A) => Boolean): Boolean = {
     if(arr.length < 2){
        return  true
     }
     val start = 0;
     val size = arr.length;
     go (start,size, arr, sorting);
  }

  @tailrec
  private def go[A](curr: Int, size: Int, arr: Array[A], sorting: (A,A) => Boolean) : Boolean = {

    if(!sorting(arr(curr), arr(curr+1))) {
      return false
    }

    if(curr == size - 2){
      return true
    }

    go(curr+1, size, arr,sorting);
  }

  def main (args : Array[String]): Unit ={


    def sorting (first: String, second: String) : Boolean =  if (first.compareTo(second) > 0 ){true} else {false};
    def sortingInt (first: Int, second: Int) : Boolean =  if (first <= second ){true} else {false};


    //print(isSorted[String](Array("caca", "pipi", "popo", "prrrrout"), sorting));

    print(isSorted[Int](Array(3,2), sortingInt));
  }



}

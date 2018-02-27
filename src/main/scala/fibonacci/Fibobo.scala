package fibonacci

import scala.annotation.tailrec

/**
  * Created by damiencharpentier on 17-11-01.
  */
object Fibobo {

  def fib(n: Int): Int = {
    if(n==0){
      return 0
    }
    if(n==1){
      return 1
    }
    go(1,1,2,n)
  }

   @tailrec
   def go (first: Int, second: Int, nb: Int,  max: Int): Int = {
     if(nb == max ){
        return first + second
     }

     go(second, first + second, nb+1, max)

  }

  def main (args : Array[String]): Unit ={
    print(fib(7));
  }

}

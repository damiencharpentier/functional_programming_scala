package chapter_6

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG  {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

}

object PureApi {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val result = rng.nextInt
    if(result._1 < 1){
      (-result._1, result._2)
    } else if(result._1 == Int.MinValue) {
      (0, result._2)
    } else {
      result
    }
  }

  /**
    * 6.2
    * @param rng
    * @return
    */
  def double(rng: RNG): (Double, RNG) = {
    val result = nonNegativeInt(rng)
    val doubleValue: Double = (result._1 / Int.MaxValue).toDouble
    (doubleValue,result._2)
  }

  /**
    * 6.3
    * @param rng
    * @return
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val firstResult = nonNegativeInt(rng)
    val secondResult = double(firstResult._2)
    ((firstResult._1,secondResult._1),secondResult._2)
  }

  /**
    * 6.3
    * @param rng
    * @return
    */
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val firstResult = double(rng)
    val secondResult = nonNegativeInt(firstResult._2)
    ((firstResult._1,secondResult._1),secondResult._2)
  }

  /**
    * 6.3
    * @param rng
    * @return
    */
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val firstResult = double(rng)
    val secondResult = double(firstResult._2)
    val thirdResult = double((secondResult._2))
    ((firstResult._1,secondResult._1, thirdResult._1),secondResult._2)
  }

  /**
    * 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def go(current: (List[Int], RNG), rng: RNG, count: Int): (List[Int], RNG) = {
      if (count == 0) {
        (current._1.reverse, current._2)
      } else {
        val next = rng.nextInt
        go((next._1 :: current._1, next._2), next._2, count - 1)
      }
    }

    if (count == 0) {
      return (List(): List[Int], rng)
    } else {
      val next = rng.nextInt
      go((next._1 :: List(), next._2), next._2, count - 1)
    }
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  /**
    * exercise 6.5
    */
  def doubleMap: Rand[Double] = {
    map(nonNegativeInt)(i => (i  / (Int.MaxValue.toDouble + 1)))
  }

  /** exercise 6.6
    *
    * @param ra
    * @param rb
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>  {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a,b), rngB)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((_,_))
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

}

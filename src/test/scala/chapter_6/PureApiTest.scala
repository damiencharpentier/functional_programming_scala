package chapter_6

import org.scalatest.FunSuite

class PureApiTest extends FunSuite  {

  test("list of integers") {
    val startRng = SimpleRNG(1)
    val result: (List[Int], RNG) = PureApi.ints(5)(startRng)
    assert(result._1.size == 5)
    assert(result._1.head == startRng.nextInt._1)
    assert(result._1.last == startRng.nextInt._2.nextInt._2.nextInt._2.nextInt._2.nextInt._1)
    assert(result._2 == startRng.nextInt._2.nextInt._2.nextInt._2.nextInt._2.nextInt._2)
  }

}

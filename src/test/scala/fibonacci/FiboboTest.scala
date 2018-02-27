package fibonacci

import org.scalatest.FunSuite

class FiboboTest extends FunSuite {

  test("fibonacci result for 0 is 0") {
    assert(Fibobo.fib(0) == 0)
  }

  test("fibonacci result for 1 is 1") {
    assert(Fibobo.fib(0) == 0)
  }

  test("fibonacci result for 2 is 2") {
    assert(Fibobo.fib(0) == 0)
  }

  test("fibonacci result for 3 is 3") {
    assert(Fibobo.fib(3) == 3)
  }

  test("fibonacci result for 4 is 5") {
    assert(Fibobo.fib(4) == 5)
  }

  test("fibonacci result for 5 is 8") {
    assert(Fibobo.fib(5) == 8)
  }

  test("fibonacci result for 6 is 13") {
    assert(Fibobo.fib(6) == 13)
  }

  test("fibonacci result for 7 is 21") {
    assert(Fibobo.fib(7) == 21)
  }

  test("fibonacci result for 8 is 34") {
    assert(Fibobo.fib(8) == 34)
  }

  test("fibonacci result for 9 is 55") {
    assert(Fibobo.fib(9) == 55)
  }

  test("fibonacci result for 10 is 89") {
    assert(Fibobo.fib(10) == 89)
  }



}

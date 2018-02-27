package fibonacci.sorted

import fibonacci.sorted.Sorted.isSorted
import org.scalatest.FunSuite

class SortedTest extends FunSuite {

  test("sorted integers") {
    def sortingInt(first: Int, second: Int): Boolean = if (first <= second) {
      true
    } else {
      false
    };

    assert(isSorted[Int](Array(2, 3, 4, 5), sortingInt))
  }

  test("not sorted integers") {
    def sortingInt(first: Int, second: Int): Boolean = if (first <= second) {
      true
    } else {
      false
    };

    assert(!isSorted[Int](Array(1, 2, 3, 2), sortingInt))
  }


  test("not sorted strings") {
    def sorting(first: String, second: String): Boolean = if (first.compareTo(second) > 0) {
      true
    } else {
      false
    };

    assert(!isSorted[String](Array("fafa", "fifi", "fofo", "frrrrout"), sorting))
  }

  test("sorted strings") {
    def sorting(first: String, second: String): Boolean = if (first.compareTo(second) > 0) {
      true
    } else {
      false
    };

    assert(isSorted[String](Array("d","c", "b", "a"), sorting))
  }

}
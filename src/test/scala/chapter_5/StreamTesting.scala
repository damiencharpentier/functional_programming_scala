package chapter_5

import org.scalatest._

class StreamTesting extends FunSuite {


  test("Transforming empty stream to list should be equal to an empty list") {
    val st = Stream.empty
    assert(st.toList() == List.empty)
  }

  test("Transforming stream with only one element = 'a' should be equal to a list of a single element = 'a'") {
    val st = Stream.cons("a",Stream.empty)
    assert(st.toList() == List("a"))
  }

  test("dropping one element of a stream of one element should equal to an empty list") {
    val st = Stream.cons("a",Stream.empty)
    assert(st.drop(1).toList() == List.empty)
  }

  test("taking one element of a stream of 'a' and 'b' elements should equal to list containing 'a' as element") {
    val st = Stream.cons("a",Stream.cons("b",Stream.empty))
    assert(st.take(1).toList() == List("a"))
  }

  test("taking while elements from the stream that equals 'a' should equal to list containing 'a' as element") {
    val st = Stream.cons("a",Stream.cons("b",Stream.empty))
    assert(st.takeWhile( _ == "a" ).toList() == List("a"))
  }

  test("taking while elements from the stream that equals 'c' should equal to empty list") {
    val st = Stream.cons("a",Stream.cons("b",Stream.empty))
    assert(st.takeWhile( _ == "c" ).toList() == List.empty)
  }

}
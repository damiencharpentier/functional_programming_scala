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

  test("stream of 1 2 3 contains odd elements") {
    val st = Stream.cons(1,Stream.cons(2,Stream.cons(3,Stream.empty)))
    assert(!st.forAll( _ % 2 == 0))
  }

  test("stream of 2 4 6 8 contains only even elements") {
    val st = Stream.cons(2,Stream.cons(4,Stream.cons(6,Stream.cons(8,Stream.empty))))
    assert(st.forAll( _ % 2 == 0))
  }

  test("taking while elements using fold right from the stream that equals 'a' should equal to list containing 'a' as element") {
    val st = Stream.cons("a",Stream.cons("b",Stream.empty))
    assert(st.takeWhileFoldRight( _ == "a" ).toList() == List("a"))
  }

  test("taking while elements using fold right from the stream that equals 'a' or 'b' should equal to list containing 'a' and 'b' as elements") {
    val st = Stream.cons("a",Stream.cons("b",Stream.empty))
    assert(st.takeWhileFoldRight( c => (c == "a" || c == "b") ).toList() == List("a","b"))
  }

  test("taking while elements using fold right from the stream that equals 'c' should equal to empty list") {
    val st = Stream.cons("a",Stream.cons("b",Stream.empty))
    assert(st.takeWhileFoldRight( _ == "c" ).toList() == List.empty)
  }

  test("map using foldRight, add 1 to each element") {
    val st = Stream.cons(1,Stream.cons(2,Stream.cons(4,Stream.empty)))
    assert(st.map[Int](x => x + 1).toList() == List(2,3,5))
  }

  test("filter using foldRight, keep only odd elements") {
    val st = Stream.cons(1,Stream.cons(2,Stream.cons(4,Stream.empty)))
    assert(st.filter(x => x % 2 == 1).toList() == List(1))
  }


  test("append using foldRight") {
    val st = Stream.cons(1,Stream.cons(2,Stream.cons(4,Stream.empty)))
    val st2 = Stream.cons(10,Stream.cons(11,Stream.cons(12,Stream.empty)))
    assert(st.append(st2).toList() == List(1, 2, 4, 10, 11, 12))
  }


  test("flatmap using foldRight") {
    val st = Stream.cons(1,Stream.cons(2,Stream.cons(4,Stream.empty)))
    assert(st.flatMap[Int](elem => Stream.cons(elem + 1, Stream.empty)).toList() == List(2, 3, 5))
  }

}
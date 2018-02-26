package chapter_5

import org.scalatest._

class FirstSpec extends FunSuite {

  override def withFixture(test: NoArgTest) = { // Define a shared fixture
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    val st = Stream.empty
    assert(st.toList() == List.empty)
  }

}
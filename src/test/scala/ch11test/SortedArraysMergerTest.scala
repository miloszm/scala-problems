package ch11test

import org.scalatest.{FunSpec, Matchers}
import ch11.SortedArraysMerger.merge

/**
  * 11.1 test
  */
class SortedArraysMergerTest extends FunSpec with Matchers {

  def fixture =
    new {
      val a = Array.ofDim[Int](11)
      val b = Array(2,4,6,8,10)
      for (i <- 0 to 5) a(i) = i*2+1
    }

  describe("two sorted arrays when merged") {
    it("should be merged into a sorted array") {
      val f = fixture
      merge(f.a, f.b, 6, 5) should contain theSameElementsInOrderAs(1 to 11)
    }
    it("should contain all elements from merged arrays") {
      val f = fixture
      merge(f.a, f.b, 6, 5) should contain allElementsOf((f.a.toList ++ f.b.toList).filter(_ != 0))
    }
    it("should not modify the first array") {
      val f = fixture
      f.a.filter(_ != 0) should contain theSameElementsInOrderAs(List(1,3,5,7,9,11))
    }
    it("should not modify the second array") {
      val f = fixture
      f.b should contain theSameElementsInOrderAs(List(2,4,6,8,10))
    }
  }

}
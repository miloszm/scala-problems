package ch11test

import ch11.LongestIncreasingSubsequenceFinder.HtWt
import ch11.LongestIncreasingSubsequenceFinder._
import org.scalatest.{FunSpec, Matchers}

/**
  * 11,7 test
  */
class LongestIncreasingSubsequenceFinderTest extends FunSpec with Matchers {

  describe("longest increasing two-dimensional subsequence") {
    it("should be correct") {

      val input = List(HtWt(65, 100), HtWt(70, 150), HtWt(56, 90), HtWt(75, 190), HtWt(60, 95), HtWt(68, 110))

      longestIncreasingSubsequence(input) should contain theSameElementsInOrderAs List(HtWt(75,190), HtWt(70,150), HtWt(68,110), HtWt(65,100), HtWt(60,95), HtWt(56,90))

    }
  }

}

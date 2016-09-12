package ch11test

import ch11.LongestIncreasingSubsequenceFinder.HtWt
import ch11.LongestIncreasingSubsequenceFinder._
import org.scalatest.{FlatSpec, FunSpec, Matchers}

/**
  * 11.7 test
  */
class LongestIncreasingSubsequenceFinderTest extends FlatSpec with Matchers {

  "longest increasing subsequence" should "find longest subsequence of weights and heights" in {
      val input = List(HtWt(65, 100), HtWt(70, 150), HtWt(56, 90), HtWt(75, 190), HtWt(60, 95), HtWt(68, 110))
      longestIncreasingSubsequenceSorted(input) should contain theSameElementsInOrderAs
        List(HtWt(56,90), HtWt(60,95), HtWt(65,100), HtWt(68,110), HtWt(70,150), HtWt(75,190))
   }

  "longest increasing subsequence" should "find longest subsequence of weights and heights 2" in {
    val input = List(HtWt(1, 1),HtWt(1, 2),HtWt(1, 3),HtWt(1, 2),HtWt(1, 3),HtWt(1, 4),HtWt(1, 5),HtWt(1, 1),HtWt(1, 2),HtWt(1, 3),HtWt(2, 1),HtWt(1, 1))
    longestIncreasingSubsequenceSorted(input) should contain theSameElementsInOrderAs
      List(HtWt(1, 2),HtWt(1, 3),HtWt(1, 4),HtWt(1, 5))
  }

  "longest increasing subsequence" should "find longest subsequence of 1 in" in {
    val input = List(1)
    longestIncreasingSubsequence(input) should contain theSameElementsInOrderAs List(1)
  }

  "longest increasing subsequence" should "find longest subsequence of 1,2,3 in" in {
    val input = List(1, 2, 3)
    longestIncreasingSubsequence(input) should contain theSameElementsInOrderAs List(1,2,3)
  }

  "longest increasing subsequence" should "find longest subsequence of 1,2,3,1,2 in" in {
    val input = List(1, 2, 3, 1, 2)
    longestIncreasingSubsequence(input) should contain theSameElementsInOrderAs List(1,2,3)
  }

  "longest increasing subsequence" should "find longest subsequence of 1,2,3,2,3,4,5,6,1 in" in {
    val input = List(1, 2, 3, 2, 3, 4, 5, 6, 1)
    longestIncreasingSubsequence(input) should contain theSameElementsInOrderAs List(2, 3, 4, 5, 6)
  }

  "longest increasing subsequence" should "find longest subsequence of 13,14,10,11,12 in" in {
    val input = List(13,14,10,11,12)
    longestIncreasingSubsequence(input) should contain theSameElementsInOrderAs List(10,11,12)
  }

}


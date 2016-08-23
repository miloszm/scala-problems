package ch11test

import ch11.RankNode
import org.scalatest.{FunSpec, Matchers}

/**
  * 11.8 test
  */
class IntStreamRankerTest extends FunSpec with Matchers {


  describe("ranks of elements of integer stream") {
    it("should be correct") {
      val list = List(5,1,4,4,5,9,7,13,3)

      val root = RankNode(0, list(0), None, None)

      for (e <- list.drop(1)) root.insert(e)

      root.getRank(1) should be (0)
      root.getRank(3) should be (1)
      root.getRank(4) should be (3)
    }
  }

}

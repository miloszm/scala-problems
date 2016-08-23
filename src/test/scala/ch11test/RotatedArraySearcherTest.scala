package ch11test

import org.scalatest.{FunSpec, Matchers}
import ch11.RotatedArraySearcher.search

/**
  * 11.3 test
  */
class RotatedArraySearcherTest extends FunSpec with Matchers {

  describe("index of element in sorted but rotated array") {
    it ("should be found if element is present") {

      search(Array(15,16,19,20,25,1,3,4,5,7,10,14), 0, 11, 5) should be (Some(8))

    }
    it ("should be none of element is not present") {

      search(Array(15,16,19,20,25,1,3,4,5,7,10,14), 0, 11, 6) should be (None)

    }
  }

}

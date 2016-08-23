package ch05test

import org.scalatest.{FunSpec, Matchers}
import ch05.OnesCounter._
import IntToBaseUtil._

/**
  * 5,5 test
  */

object IntToBaseUtil {
  implicit class IntToBase( val digits:String ) extends AnyVal {
    def base(b:Int) = Integer.parseInt( digits, b )
    def b = base(2)
  }
}

class OnesCounterTest extends FunSpec with Matchers {

  describe("number of ones in an integer") {
    it("should be correctly calculated") {
      countOnes("000000000000".b) should be (0)
      countOnes("110110111011".b) should be (9)
      countOnes("111111111111".b) should be (12)
      countOnes("100000000000".b) should be (1)
      countOnes("100000000001".b) should be (2)
    }
  }

}

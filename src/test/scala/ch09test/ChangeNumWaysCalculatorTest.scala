package ch09test

import ch09.ChangeNumWaysCalculator._
import org.scalatest.{FunSpec, Inside, Matchers}


/**
  * 9.8 Test
  */
class ChangeNumWaysCalculatorTest extends FunSpec with Matchers with Inside {

  describe("number of ways change can be given with 4 coins") {
    it("should be correct") {
      val coins = List(1,5,10,25)
      val tasks:List[(Int,Int)] = List(50,100,150,200,250,300).zip(List(49,242,680,1463,2691,4464))
      tasks.foreach{ t =>
        numChange(coins,t._1) shouldEqual t._2
      }
    }
  }

  describe("number of ways change can be given with 5 coins") {
    it("should be correct") {
      val coins = List(1,5,10,25,50)
      val tasks = List(50,100,150,200,250,300).zip(List(50,292,972,2435,5126,9590))
      tasks.foreach{ t =>
        numChange(coins,t._1) shouldEqual t._2
      }
    }
  }

}

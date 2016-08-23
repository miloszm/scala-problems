package ch11test

import org.scalatest.{FunSpec, Matchers}
import ch11.MatrixSearcher._

/**
  * 11.6 test
  */
class MatrixSearcherTest extends FunSpec with Matchers {

  describe("matrix element found") {
    it ("should be correct") {

      val a: Array[Array[Int]] = Array (
      Array (15, 20, 70, 85),
      Array (20, 35, 80, 95),
      Array (30, 55, 95, 105),
      Array (40, 80, 120, 120) )

      val c = findElement (Matrix (a, Coord (0, 0), Coord (3, 3) ), 55)
      c should be (Some(Coord(2,1)))
    }
  }

}

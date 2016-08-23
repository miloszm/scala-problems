package ch05

import scala.annotation.tailrec

/**
  * 5.5
  */
object OnesCounter {

  def countOnes(n:Int): Int = {
    @tailrec
    def go(n:Int, acc:Int): Int = if (n == 0) acc else go(n & n-1, acc + 1)
    go(n, 0)
  }

}

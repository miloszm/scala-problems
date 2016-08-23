package ch11

import scala.annotation.tailrec

/**
  * 11.1
  */
object SortedArraysMerger {

  def merge(a :Array[Int], b:Array[Int], lastA: Int, lastB: Int):Array[Int] = {
    @tailrec
    def go(a: Array[Int], b: Array[Int], indexA: Int, indexB: Int, indexMerged: Int):Unit = {
      if (indexB >= 0) {
        if (indexA >= 0 && a(indexA) > b(indexB)) {
          a(indexMerged) = a(indexA)
          go(a,b,indexA-1,indexB,indexMerged-1)
        }
        else {
          a(indexMerged) = b(indexB)
          go(a,b,indexA,indexB-1,indexMerged-1)
        }
      }
    }
    val c = a.clone()
    go(c,b,lastA-1, lastB-1, lastB + lastA - 1)
    c
  }

}
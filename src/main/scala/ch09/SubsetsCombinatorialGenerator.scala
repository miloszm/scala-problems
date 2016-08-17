package ch09

/**
  * 9.4
  */
object SubsetsCombinatorialGenerator {

  def isBitSet(i:Int, k:Int):Boolean = {
    (i & (1 << k)) != 0
  }

  def convertToSet(set:List[Int], i:Int):List[Int] = {
    (for {
      k <- (set.size-1) to 0 by -1
      if (isBitSet(i, k))
    }
    yield{
      set(k)
    }).toList
  }

  def generateSubsets(set:List[Int]): List[List[Int]] = {
    val max = 1 << set.size
    (for (i <- 0 to (max-1)) yield convertToSet(set, i)).toList
  }

}

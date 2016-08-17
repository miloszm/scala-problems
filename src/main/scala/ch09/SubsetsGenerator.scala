package ch09

/**
  * 9.4
  */
object SubsetsGenerator {

  def generateSubsets(set:List[Int]): List[List[Int]] = {
    def go(set:List[Int], index:Int): List[List[Int]] = {
      if (index > 0) {
        val all = go(set, index-1)
        val item = set(index-1)
        val moreSubsets =
          for {subset <- all}
          yield {
            item :: subset
          }
        all ::: moreSubsets
      }
      else {
        List(List())
      }
    }
    go(set, set.size)
  }

}

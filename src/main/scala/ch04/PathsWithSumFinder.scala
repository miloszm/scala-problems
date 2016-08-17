package ch04

/**
  * 4.9
  */
object PathsWithSumFinder {

  def findPaths(node:TreeNode, sum:Int): List[List[Int]] = {
    def go(node:Option[TreeNode], sum:Int, path:List[Int]):List[List[Int]] = {
      if (node.isDefined){
        val p = node.get.data +: path
        val im = p.scanLeft(0)(_ + _)
        val slices =
          for {
            i <- 1 to p.length
            if (im(i) == sum)
          }
          yield {
            p.take(i)
          }
        slices.toList ::: go(node.get.left, sum, p) ::: go(node.get.right, sum, p)
      }
      else {
        List()
      }
    }
    go(Some(node), sum, List())
  }

}

package ch04

/**
  * 4.1
  */
object TreeBalanceChecker {

  def isBalanced(root:TreeNode):Boolean = {
    def checkBalance(n:Option[TreeNode]): Option[Int] = {
      if (n.isEmpty){
        Some(0)
      }
      else {
        val x = for {
          l <- checkBalance(n.get.left)
          r <- checkBalance(n.get.right)
        } yield {
          (l, r)
        }
        if (x.isEmpty) {
          None
        }
        else {
          if (Math.abs(x.get._1 - x.get._2) > 1) {
            None
          }
          else {
            Some(Math.max(x.get._1, x.get._2) + 1)
          }
        }
      }
    }
    checkBalance(Some(root)).isDefined
  }

}

package ch04

/**
  * 4.1
  */
object TreeBalanceChecker {

  def isBalanced(root:TreeNode):Boolean = {
    TreeNode.checkBalance(Some(root)).isDefined
  }

}

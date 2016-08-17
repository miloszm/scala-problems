package ch04test

import ch04.TreeBalanceChecker._
import ch04.TreeNode
import org.scalatest.{FunSpec, Matchers}


/**
  * 4.1 Test
  */
class TreeBalanceCheckerTest extends FunSpec with Matchers {

  describe("tree in which left subtree height equals right subtree height or is different by 1 only") {
    it("should be balanced if all nodes are balanced") {
      val list = List(5,3,2,4,7,6,9,8,10)
      val root = TreeNode(list(0), None, None)
      for (e <- list.drop(1)) root.insert(e)
      isBalanced(root) should be (true)
    }
    it("should not be balanced if at least one node is not balanced") {
      val list = List(5,3,2,4,7,6,9,8,10,11,12)
      val root = TreeNode(list(0), None, None)
      for (e <- list.drop(1)) root.insert(e)
      isBalanced(root) should be (false)
    }
  }

}

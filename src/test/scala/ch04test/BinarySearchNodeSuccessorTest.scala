package ch04test

import ch04.BinarySearchNodeSuccessor.TreeNode
import ch04.BinarySearchNodeSuccessor.inorderSucc
import org.scalatest.{FunSpec, Inside, Matchers}


/**
  * 4.6 Test
  */
class BinarySearchNodeSuccessorTest extends FunSpec with Matchers with Inside {

  private def insert(n:TreeNode, d:Int):Unit = {
    if (d <= n.data){
      if (n.left.isDefined) insert(n.left.get, d)
      else n.left = Some(TreeNode(d, None, None, Some(n)))
    }
    else {
      if (n.right.isDefined) insert(n.right.get, d)
      else n.right = Some(TreeNode(d, None, None, Some(n)))
    }
  }

  private def find(n:Option[TreeNode], d:Int):Option[TreeNode] = {
    if (n.isDefined && n.get.data == d) n
    else if (!n.isDefined) None
    else if (d < n.get.data){
      find(n.get.left, d)
    }
    else {
      find(n.get.right, d)
    }
  }

  private def doInorderSucc(n:Int):Option[Int] = {
    val nNode: TreeNode = find(Some(root), n).getOrElse(TreeNode(root.data, None, None, None))
    val succ = inorderSucc(nNode)
    succ.map(_.data)
  }


  /**
    *             5
    *           /   \
    *          3     7
    *         / \   / \
    *        2  4  6   9
    *                 / \
    *                8  10
    */

  val Root = 5
  val NotPresent = 20
  val list = List(Root,3,2,4,7,6,9,8,10)
  val root = TreeNode(list(0), None, None, None)
  for (e <- list.drop(1)) insert(root, e)

  describe("in-order successor of node") {
    it("should be leftmost child of right subtree") {
      doInorderSucc(5) should be (Some(6))
    }
    it("should be right parent recursively") {
      doInorderSucc(4) should be (Some(5))
    }
    it("should be right parent") {
      doInorderSucc(2) should be (Some(3))
    }
    it("should be none if right subtree is none and no right parent recursively") {
      doInorderSucc(10) should be (None)
    }
    it("should be right parent recursively (2)") {
      doInorderSucc(6) should be (Some(7))
    }
    it("should be leftmost child of right subtree (2)") {
      doInorderSucc(9) should be (Some(10))
    }
    it("should be none if not present") {
      doInorderSucc(NotPresent) should be (None)
    }
  }

}

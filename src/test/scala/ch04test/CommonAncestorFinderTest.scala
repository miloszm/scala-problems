package ch04test

import ch04.TreeNode
import org.scalatest.{FunSpec, Inside, Matchers}
import ch04.CommonAncestorFinder._


/**
  * 4.7 Test
  */
class CommonAncestorFinderTest extends FunSpec with Matchers with Inside {

  private def doFindCommonAncestor(root:TreeNode, p:Int, q:Int):Option[Int] = {
    val pNode: TreeNode = TreeNode.find(Some(root), p).getOrElse(TreeNode(root.data, None, None))
    val rNode: TreeNode = TreeNode.find(Some(root), q).getOrElse(TreeNode(root.data, None, None))
    val ancestor = findCommonAncestor(root, pNode, rNode)
    ancestor.map(_.data)
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
  val root = TreeNode(list(0), None, None)
  for (e <- list.drop(1)) root.insert(e)

  describe("common ancestor of two nodes") {
    it("should be found in case it is not a root") {
      doFindCommonAncestor(root, 6, 10) should be (Some(7))
    }
    it("should be found in case it is a root") {
      doFindCommonAncestor(root, 2, 10) should be (Some(Root))
    }
    it("should be root in case one of them is root") {
      doFindCommonAncestor(root, Root, 10) should be (Some(Root))
    }
    it("should be root in case both of them are root") {
      doFindCommonAncestor(root, Root, Root) should be (Some(Root))
    }
    it("should be none if one node is not present") {
      doFindCommonAncestor(root, 6, NotPresent) should be (None)
    }
    it("should be none if one node is root and the other is not present") {
      doFindCommonAncestor(root, Root, NotPresent) should be (None)
    }
    it("should be none if the other node is not present") {
      doFindCommonAncestor(root, NotPresent, 6) should be (None)
    }
    it("should be none if one node is not present and the other is root") {
      doFindCommonAncestor(root, NotPresent, Root) should be (None)
    }

  }

}

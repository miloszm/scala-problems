package ch04test

import ch04.TreeNode
import org.scalatest.{FunSpec, Inside, Matchers}
import ch04.PathsWithSumFinder._


/**
  * 4.9 Test
  */
class PathsWithSumFinderTest extends FunSpec with Matchers with Inside {

  describe("paths with nodes whose values sum to a given sum") {
    it("should contain specific paths") {
      val list = List(5,3,2,4,7,6,9,8,10)
      val root = TreeNode(list(0), None, None)
      for (e <- list.drop(1)) root.insert(e)

      findPaths(root, 13) should contain(List(6,7))
      findPaths(root, 8) should contain allOf (List(3,5), List(8))
      findPaths(root, 8) should contain(List(8))
    }
  }

  describe("paths with nodes with negative values") {
    it("should be found correctly") {
      val list = List(-3,-5,-1,5,3,2,4,7,6,9,8,10)
      val root = TreeNode(list(0), None, None)
      for (e <- list.drop(1)) root.insert(e)

      findPaths(root, -8) should contain(List(-5,-3))
      findPaths(root, 4) should contain allOf (List(5, -1), List(3, 5, -1, -3), List(4))
    }
  }

}

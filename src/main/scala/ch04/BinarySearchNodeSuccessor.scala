package ch04

import scala.annotation.tailrec

/**
  * 4.6
  */
object BinarySearchNodeSuccessor {

  case class TreeNode(data:Int, var left:Option[TreeNode], var right:Option[TreeNode], parent:Option[TreeNode]){
    @tailrec final def leftMostChild:Option[TreeNode] = {
      left match {
        case None => Some(this)
        case c:Some[TreeNode] => c.get.leftMostChild
      }
    }
    @tailrec final def topLeftOrFirstRightParent:Option[TreeNode] = {
      parent match {
        case None => None
        case p:Some[TreeNode] => if (p.get.left == Some(this)) p else p.get.topLeftOrFirstRightParent
      }
    }
  }

  def inorderSucc(n:TreeNode):Option[TreeNode] = {
    if (n.right.isEmpty){
      n.topLeftOrFirstRightParent
    }
    else {
      n.right.get.leftMostChild
    }
  }

}

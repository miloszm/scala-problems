

package object ch04 {

  object TreeNode {
    def find(n:Option[TreeNode], d:Int):Option[TreeNode] = {
      if (n.isDefined && n.get.data == d) n
      else if (!n.isDefined) None
      else if (d < n.get.data){
        find(n.get.left, d)
      }
      else {
        find(n.get.right, d)
      }
    }

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
  }

  case class TreeNode(data:Int, var left:Option[TreeNode], var right:Option[TreeNode]){
    def insert(d:Int):Unit = {
      if (d <= data){
        if (left.isDefined) left.get.insert(d)
        else left = Some(TreeNode(d, None, None))
      }
      else {
        if (right.isDefined) right.get.insert(d)
        else right = Some(TreeNode(d, None, None))
      }
    }
  }

}

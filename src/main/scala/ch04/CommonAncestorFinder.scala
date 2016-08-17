package ch04

/**
  * 4.7
  */
object CommonAncestorFinder {

  case class Result(node:Option[TreeNode], isAncestor:Boolean)

  def bothFound(l:Result, r:Result) = l.node.isDefined && r.node.isDefined
  def oneFound(l:Result, r:Result) = l.node.isDefined || r.node.isDefined

  def findCommonAncestor(root:TreeNode, p:TreeNode, q:TreeNode):Option[TreeNode] = {
    def go(root:Option[TreeNode], p:TreeNode, q:TreeNode):Result = {
      if (root.isEmpty){
        Result(None, false)
      }
      else if (root == Some(p) && root == Some(q)){
        Result(root, true)
      }
      else {
        val rl = go(root.get.left, p, q)
        if (rl.isAncestor) {
          rl
        }
        else {
          val rr = go(root.get.right, p, q)
          if (rr.isAncestor) {
            rr
          }
          else if (bothFound(rl, rr)) {
            Result(root, true)
          }
          else if (root.get == p || root.get == q) {
            Result(root, oneFound(rl, rr))
          }
          else {
            Result(rl.node.orElse(rr.node), false)
          }
        }
      }
    }
    val r = go(Some(root), p, q)
    if (r.isAncestor) r.node else None
  }
}

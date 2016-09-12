package ch11

/**
  * 11.7
  */
object LongestIncreasingSubsequenceFinder {

  case class HtWt(ht:Int, wt:Int) extends Ordered[HtWt]{
    override def compare(that: HtWt): Int = {
      wt.compare(that.wt)
    }
  }

  /**
    * For two dimensional problem we sort on one dimension (height)
    * and then we search longest increasing subsequence on the other dimension (weight).
    */
  def longestIncreasingSubsequenceSorted(array: List[HtWt]): List[HtWt] = {
    longestIncreasingSubsequence(array.sortBy(_.ht))
  }

  def longestIncreasingSubsequence[T:Ordering](array: List[T])(implicit ord:Ordering[T]): List[T] = {
    def mx(a:List[T],b:List[T]):List[T] = if (a.length > b.length) a else b
    def go(a: List[T], cur: List[T], prev: List[T]): List[T] = {
      a match {
        case Nil => mx(cur,prev).reverse
        case h :: t =>
          if (cur.isEmpty || ord.gt(h,cur.head)) {
            go(t, h :: cur, prev)
          }
          else {
            go(t, List(h), mx(cur,prev))
          }
      }
    }
    go(array, List(), List())
  }
}

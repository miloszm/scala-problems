package ch11

import scala.annotation.tailrec

/**
  * 11.3
  */
object RotatedArraySearcher {

  implicit class Between(x:Int) {
    def between(l: Int, r: Int): Boolean = { x >= l && x <= r }
  }

  @tailrec
  def search(a:Array[Int], left:Int, right:Int, x:Int): Option[Int] = {
    val mid = (left + right) / 2
    if (x == a(mid)){
      Some(mid)
    }
    else if (right < left) {
      None
    }
    else {
      if (a(left) < a(mid)){
        if (x.between(a(left), a(mid))) search(a, left, mid-1, x)
        else search(a, mid+1, right, x)
      }
      else if (a(left) > a(mid)){
        if (x.between(a(mid), a(right))) search(a, mid+1, right, x)
        else search(a, left, mid-1, x)
      }
      else {
        search(a, mid+1, right, x)
      }
    }
  }

}

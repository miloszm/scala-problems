package ch11

/**
  * 11.8
  */
case class RankNode(var leftSize: Int, data: Int, var left: Option[RankNode], var right: Option[RankNode]) {
  def insert(d: Int): Unit = {
    if (d <= data) {
      if (left.isDefined) left.get.insert(d)
      else left = Some(RankNode(0, d, None, None))
      leftSize += 1
    }
    else {
      if (right.isDefined) right.get.insert(d)
      else right = Some(RankNode(0, d, None, None))
    }
  }

  def getRank(d: Int): Int = {
    if (d == data) {
      leftSize
    }
    else if (d <= data) {
      left.map(_.getRank(d)).getOrElse(-1)
    } else {
      right.map(_.getRank(d) + 1 + leftSize).getOrElse(-1)
    }
  }
}

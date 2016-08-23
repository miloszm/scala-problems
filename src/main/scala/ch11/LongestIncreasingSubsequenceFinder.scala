package ch11

/**
  * 11.7
  */
object LongestIncreasingSubsequenceFinder {

  case class HtWt(ht:Int, wt:Int) extends Ordered[HtWt]{
    override def compare(that: HtWt): Int = {
      if (ht != that.ht) ht.compare(that.ht) else wt.compare(that.wt)
    }
    def isBefore(other:HtWt):Boolean = ht < other.ht && wt < other.wt
  }

  def longestIncreasingSubsequence(array:List[HtWt]):List[HtWt] = {
    val arraySorted = array.sorted
    var solutions:List[List[HtWt]] = List()
    for (currentElement <- arraySorted){
      val bestSequenceCandidates = solutions.filter(_.last.isBefore(currentElement))
      val bestSequence:List[HtWt] = if (bestSequenceCandidates.isEmpty) List() else bestSequenceCandidates.maxBy(_.length)
      val newSolutions = (currentElement :: bestSequence) :: solutions
      solutions = newSolutions
    }
    solutions.maxBy(_.length)
  }

}

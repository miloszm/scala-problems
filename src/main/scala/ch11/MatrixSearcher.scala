package ch11

/**
  * 11.6
  */
object MatrixSearcher {

  case class Matrix(m:Array[Array[Int]], orig:Coord, dest:Coord) {
    private def inbounds(c:Coord):Boolean = {
      c.row >= 0 && c.col >= 0 && c.row < m.length && c.col < m(0).length
    }
    def isInbounds: Boolean = {
      inbounds(orig) && inbounds(dest)
    }
    def apply(c: Coord): Int = {
      m(c.row)(c.col)
    }
    def lowerLeft(pivot:Coord):Matrix = {
      new Matrix(m, Coord(pivot.row, orig.col), Coord(dest.row, pivot.col -1))
    }
    def upperRight(pivot:Coord):Matrix = {
      new Matrix(m, Coord(orig.row, pivot.col), Coord(pivot.row-1, dest.col))
    }
    val origBeforeDest = orig.isBefore(dest)
    val origElem = apply(orig)
  }

  object Coord {
    def avg(min:Coord, max:Coord):Coord = {Coord((min.row + max.row)/2, (min.col + max.col)/2)}
  }

  case class Coord(row:Int, col:Int) {
    def isBefore(p:Coord):Boolean = {row < p.row && col < p.col}
    def diagonalDistance(p:Coord):Int = Math.min(Math.abs(row-p.row), Math.abs(col-p.col))
    def add(n:Int):Coord = { new Coord(row + n, col + n) }
  }

  def findElement(m:Array[Array[Int]], x:Int):Option[Coord] = {
    findElement(Matrix(m, Coord(0,0), Coord(m.length - 1, m(0).length - 1)), x)
  }

  def searchDiagonal(m:Matrix, x:Int): Coord = {
    var start = m.orig
    var end = start.add(m.orig.diagonalDistance(m.dest))
    while (start.isBefore(end)) {
      val p = Coord.avg(start, end)
      if (x > m(p)) {
        start = p.add(1)
      }
      else {
        end = p.add(-1)
      }
    }
    start
  }

  def findElement(m:Matrix, x:Int):Option[Coord] = {
    if (!m.isInbounds){
      None
    }
    else if (m.origElem == x){
      Some(m.orig)
    }
    else if (!m.origBeforeDest){
      None
    }
    else {
      val pivot: Coord = searchDiagonal(m, x)
      findElement(m.lowerLeft(pivot), x) match {
        case c: Some[Coord] => c
        case None => findElement(m.upperRight(pivot), x)
      }
    }
  }

}

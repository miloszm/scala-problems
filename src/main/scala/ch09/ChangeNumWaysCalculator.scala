package ch09

/**
  * 9.8
  */
object ChangeNumWaysCalculator {

  def numChange(coins:List[Int], n:Int):Int = {
    def go(n: Int, coin: Int): Int = {
      val coinValue = coins(coin)
      val fullMatchFactor = if (n % coinValue == 0) 1 else 0
      val limit = (n / coinValue) - fullMatchFactor
      coinValue match {
        case 1 => 1
        case c => (for (i <- 0 to limit)
          yield {
            go(n - i * coinValue, coin - 1)
          }).sum + fullMatchFactor
      }
    }
    go(n, coins.size-1)
  }

}

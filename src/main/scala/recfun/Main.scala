package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

      def isEdge(x: Int, y: Int): Boolean = {
        x == 0 || x == y
      }

      if ( isEdge(c, r) ) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def checkParen(c: Char): Int = c match {
        case '(' =>  1
        case ')' => -1
        case _   =>  0
      }

      def checkAll(chars: List[Char], tally: Int): Boolean = {
        if (tally < 0 || chars.isEmpty) tally == 0 else checkAll(chars.tail, tally + checkParen(chars.head))
      }

      checkAll(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val sorted: List[Int]  = coins.sortWith(_ > _)

      def findPermutations(moneyLeft: Int, coinsLeft: List[Int]): Int = {
        if (coinsLeft.isEmpty || moneyLeft == 0) 0 else {

          val coin: Int = coinsLeft.head
          val newMoneyLeft: Int = moneyLeft - coin

          if (newMoneyLeft < 0) findPermutations(moneyLeft, coinsLeft.tail)
          else if (newMoneyLeft == 0) { findPermutations(moneyLeft, coinsLeft.tail) + 1 }
          else { findPermutations(newMoneyLeft, coinsLeft) + findPermutations(moneyLeft, coinsLeft.tail) }
        }
      }

      findPermutations(money, sorted)
    }
  }

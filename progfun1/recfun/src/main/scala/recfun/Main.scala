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
      if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException("Invalid value for c = " + c + ", and r = " + r)
      else if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceWithCnt(currCnt: Int, chars: List[Char]): Boolean = chars match {
        case Nil => currCnt == 0
        case head :: tail => {
          if (head == '(') balanceWithCnt(currCnt+1, tail)
          else if (head == ')' && currCnt == 0) false
          else if (head == ')' && currCnt > 0) balanceWithCnt(currCnt-1, tail)
          else balanceWithCnt(currCnt, tail)
        }
      }
      
      balanceWithCnt(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x > 0 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) + countChange(money-coins.head, coins)
    }
  }

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
   * Exercise-1:
   *
   * Pascal's triangle with number at the edges equal to 1. Each
   * number inside is the sum of two numbers above it. Function will
   * take column c and row r. Counts start from 0.
   *
   * 1
   * 1 1
   * 1 2 1
   * 1 3 3 1
   * 1 4 6 4 1
   *
   *     1
   *    1 1
   *   1 2 1
   *  1 3 3 1
   * 1 4 6 4 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || r == c) { // First and last column are 1
        1
      } else {
        // Sum of entries in  previous two rows.
        pascal(c-1,r-1) + pascal(c,r-1)
      }
    }


  /**
   * Exercise 2
   *
   * A recursive function which returns true of string represented by
   * chars has balanced paranthesis: eg. (())() or ()()
   *
   */
    def balance(chars: List[Char]): Boolean = {

      def balance_r(chars:List[Char], open: Int): Boolean = {
        chars match {
          case Nil => (open == 0)
          case c::rest => {
            c match {
              case '(' => balance_r(rest, open + 1)
              case ')' => {
                if (open == 0 ) false
                else balance_r(rest, open - 1)
              }
              case _ =>  balance_r(rest, open)
            }
          }
        }
      }
      balance_r(chars,0)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChange_r(money:Int, coins: List[Int]): Int = {
        if(money == 0) {
          1
        } else if (money > 0 && coins.isEmpty) {
          0
        } else {
          if( money < coins.head ) {
            countChange_r( money , coins.tail)
          } else {
            countChange_r( money - coins.head , coins) + countChange_r(money,coins.tail)
          }
        }
      }

      if (money  == 0 ) {
        0
      } else {
        val coinDec = coins.sortBy((x:Int) => -x)
        countChange_r(money, coinDec)
      }
    }

  }

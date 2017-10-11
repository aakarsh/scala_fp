
import org.scalameter._
import common._
import reductions._
import ParallelCountChange._

object Scratch {

  /**
   * Number of ways to change coins ... 
   */
  def main() {

    val (money: Int, coins: List[Int]) = (4, List(1,2))

    val four = countChange(money, coins, debug=true)
    println("number:" + four) // 3.0

    val fiftyChange = countChange(50, List(1, 2, 5, 10))
    println("fifty:" + fiftyChange) // 341.0

    val twoFifty    = countChange(250, List(1, 2, 5, 10, 20, 50),
                               debug=true)

    println("twoFifty:" + twoFifty) // 177863.0

  }

}

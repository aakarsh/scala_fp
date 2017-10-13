
import org.scalameter._
import common._
import reductions._
import ParallelCountChange._
import ParallelParenthesesBalancing._

object Scratch {

  def tryCountChange() {

    val (money: Int, coins: List[Int]) = (4, List(1,2))

    val four = countChange(money, coins)
    println("number:" + four) // 3.0

    val fiftyChange = countChange(50, List(1, 2, 5, 10))
    println("fifty:" + fiftyChange) // 341.0

    val twoFifty    = countChange(250, List(1, 2, 5, 10, 20, 50))
    println("twoFifty:" + twoFifty) // 177863.0

  }

  def tryParenBalance() {
    val balanced = List[String]("(if (zero? x) max (/ 1 x))",
                                  "I told him (that it's not (yet) done). (But he wasn't listening)")

    val unbalanced = List[String]("(o_()",":-)","())(",
                                  ")(","((","))",".)",".(","(.",").")
    // balance
    balanced  .foreach((s:String) => println(s"balanced:%s"  + balance(s.toArray)))
    // unbalanced
    unbalanced.foreach((s:String) => println(s"unbalanced:$s"+ !balance(s.toArray)))
  }

  def tryParallelBalance() {
    // balanced
    val balanced = List[String]("(if (zero? x) max (/ 1 x))",
                                "I told him (that it's not (yet) done). (But he wasn't listening)")
    // unbalanced
    val unbalanced = List[String]("(o_()",":-)","())(",
                                  ")(","((","))",".)",".(","(.",").")
    // parBalance     
    println("parBalance:" 
            + parBalance(unbalanced(0).toArray,threshold = 2, debug = true))
    
  }

  /**
   * Number of ways to change coins.
   */
  def main() {
    // tryCountChange()
    // tryParenBalance()
    tryParallelBalance()
  }

}

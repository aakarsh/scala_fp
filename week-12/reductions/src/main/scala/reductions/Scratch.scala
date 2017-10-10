import org.scalameter._
import common._
import reductions._

object Scratch {

  def main() {
    // Number of ways to change coins
    import ParallelCountChange._
    val (money:Int,coins:List[Int]) = (4, List(1,2))

    val number = countChange(money, coins, debug = true)
    println("Number : " + number)
 
  }
}



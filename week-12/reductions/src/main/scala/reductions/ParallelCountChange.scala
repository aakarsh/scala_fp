package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)

    val seqtime = standardConfig measure {
      seqResult =
        ParallelCountChange.countChange(amount, coins)
    }

    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {

      val fjtime = standardConfig measure {
        parResult =
          ParallelCountChange.parCountChange(amount,
                                             coins,
                                             threshold)
      }
      // Parallel Results:
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

/**
 * Returns the number of ways change can be made from the specified
 * list of coins for the specified amount of money.
 */
object ParallelCountChange {

/**
 *  [info] ParallelCountChangeSuite:
 *
 *  [info] - countChange should return 1 when money == 0 *** FAILED ***
 *  [info]   0 did not equal 1 countChang(0, _) should be 1 (ParallelCountChangeSuite.scala:31)
 *  [info] - countChange should work for multi-coins *** FAILED ***
 *  [info]   376 did not equal 341 countChange(50, List(1, 2, 5, 10)) should be 341 (ParallelCountChangeSuite.scala:63)
 */

  /**
   * Simple recursive solution is the consider the possiblity of
   * taking one coin from the least denomination.
   */
  def countChange(money: Int, coins: List[Int],
                  debug:Boolean = false,
                  maxDepth:Int = 100000): Int = {

    def aux(depth:Int, money:Int,
            coins:List[Int],
            coinsUsed:List[Int]): Int = {

      if (debug)
        println( "|"+("-"*depth) + "+" + money + " " + coins)

      if(depth == maxDepth) {
        if(debug)
          println("Reached maxDepth")
        0
      } else {
        val retval =
          coins match {
            // no make change with no coins
            case Nil    =>
              if(debug)
                println("|"+ ("-" * depth)+">"+0)
              0
            // no make change neg money
            case _ if (money < 0) =>
              if(debug)
                println("|"+("-" * depth)+">"+0)
              0
            // make chage with no coins
            case _   if (money == 0) =>
              if(debug) {
                println("|"+("-" * depth)+">"+1 +"{" + coinsUsed +"}")

              }
              1
            // make change with first coin or without it
            case firstCoin :: restCoins =>

              val takingFirstCoin   = aux(depth + 1, money - firstCoin, coins,     coinsUsed)
              val withoutFirstCoin  = aux(depth + 1, money , restCoins, firstCoin::coinsUsed)

              (withoutFirstCoin + takingFirstCoin)
          }
        retval
      }
    }
    if (money == 0) return 1
    else
      aux(0, money, coins, List[Int]())
  }

  type Threshold = (Int, List[Int]) => Boolean

  /**
   * In parallel, counts the number of ways change can be made from
   * the specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int,
                     coins: List[Int],
                     threshold: Threshold): Int = {

    // not able to balance native vs jvm stuff.
    //
    if(threshold(money,coins)) {
      countChange(money,coins)
    } else {
        val retval =
          coins match {
            // No Make change with no coins.
            case Nil  => 0
            // No Make change neg money.
            case _ if (money < 0) => 0
            // Make chage with no coins.
            case _ if (money == 0) => 1
            // Make change with first coin or without it
            case firstCoin :: restCoins =>

              // val takingFirstCoin   = aux(depth + 1, money - firstCoin, coins,     coinsUsed)
              // val withoutFirstCoin  = aux(depth + 1, money , restCoins, firstCoin::coinsUsed)
              
              val (takingFirstCoin, withoutFirstCoin) =
                parallel(parCountChange( money - firstCoin, coins, threshold),
                         parCountChange( money, restCoins, threshold))
              
              (withoutFirstCoin + takingFirstCoin)
          }
      retval
    }
  }

  /**
   * Threshold heuristic based on the starting money.
   */
  def moneyThreshold(startingMoney: Int): Threshold =
    ???

  /**
   * Threshold heuristic based on the total number of initial coins.
   */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    ???

  /**
   * Threshold heuristic based on the starting money and the initial
   * list of coins.
   */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    ???
  }
}

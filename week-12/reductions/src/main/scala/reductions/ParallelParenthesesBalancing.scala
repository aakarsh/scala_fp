package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {

    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000

    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }

    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }

    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /**
   * Returns `true` iff the parentheses
   * in the input `chars` are balanced.
   *
   * Sequentially go through the characters
   * and check that the results are
   * balanced.
   *
   */
  def balance(chars: Array[Char]): Boolean = {

    val len = chars.size

    def count(acc:Int, c: Char) =
      c match {
          case '(' => acc + 1
          // uncancellable negative
          case ')' if (acc <= 0) => -len
          case ')' => acc - 1
          case _   => acc
      }

    // detecting ')'
    (chars.foldLeft(0)(count)) == 0
  }

  /**
   * Returns `true` iff the parentheses in the input
   * `chars` are balanced.
   */
  def parBalance(chars: Array[Char],
                 threshold: Int ,
                 debug: Boolean = false): Boolean = {

    def charsPart(i:Int, j:Int) : String =
      if( i >= j) ""
      else chars.slice(i,j).foldLeft("")(_+_.toString)

    /**
     * Traverse the array starting from
     * index until limit (excluding it)
     */
    def traverse(idx:   Int, end: Int,
                 left:  Int, right: Int, depth:Int): (Int, Int) = {

      var (l:Int,r:Int) = (left,right)

      for(i <- idx until end) {
        chars(i) match {
          case '('  => l += 1          // start a parenthesis
          case ')' if l > 0  => l -= 1 // start an empty
          // no opening parenthesis on the stack
          case ')'  => r +=1 // an unbalanced right parenthesis
          case _ => // some random character
        }
      }

      if(debug)
        println(("-" * depth) + "traverse[" +(idx,end)+"]:"
                + charsPart(idx,end) + "::" + (left,right)+"==>"+(l,r))
      return (l,r)
    }

    /**
     * Takes a range [from ... end] reduces each range
     * spliting the results and computing the number of
     * unmatched left and right parentheses [..]
     */
    def reduce (from: Int, end:Int,depth:Int  = 0) : (Int, Int) = {

      if(debug)
        println(("-" * depth)+"reduce["+from+"-"+end+"]:"+charsPart(from,end))

      if(from >= end) {
        return (0, 0)
      } else if((end - from) <= threshold ) { // traverse(from, end, ... )
        // iterate and return no matching parenthesis.
        return traverse(from, end, 0, 0,depth)
      } else {
        val mid = (from + end) / 2
        val ((l1,r1),(l2,r2)) = parallel(reduce(from,mid,  depth + 1),
                                         reduce(mid ,end,  depth + 1))

        // The lesser of left parenthesis:
        // in left subtree and right parenthesis 
        // in right subtree form matches.

        val foundMatches:Int = math.min(l1,r2)

        // Match all left in left sub-tree with right parentheis in
        // right sub-tree.
        // [))  (((( ] [  )))) (((]  -> )) ((

        val unmatchedLeft  =  (l1 + l2) - foundMatches
        val unmatchedRight =  (r1 + r2) - foundMatches

        // Expected form of [)))) (((((] with matching central parenthes deleted.
        return (unmatchedLeft, unmatchedRight)
        // +--------------------------------------------------------------------+
        // | Since all matching parentheis in the subtrees are eliminatied      |
        // | only the right parentheis in left sub tree and the left parentheis |
        // | inside the right subtree will be open                              |
        // +--------------------------------------------------------------------+

      }
    }
    // given a from and a with
    reduce(0, chars.length) == (0,0)
  }

  // For those who want more: prove that your reduction operator is associative!
}

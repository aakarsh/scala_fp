package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("Test parallel balancing") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray,5) == expected ,
             s"parBalance.($input) should be $expected")

    val balanced = List[String]("(if (zero? x) max (/ 1 x))",
                                "I told him (that it's not (yet) done). (But he wasn't listening)")

    val unbalanced = List[String]("(o_()",":-)","())(")
    
    for( s <- balanced)   check(s, true)
    for( s <- unbalanced) check(s, false)

  }

}

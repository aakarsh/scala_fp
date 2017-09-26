package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { // The solution must always (lead) to legal blocks.
        case (block, move) =>
          require(block.isLegal) 
          move match {
            case Left  => block.left
            case Right => block.right
            case Up    => block.up
            case Down  => block.down
          }
      }
  }

  trait Level1 extends SolutionChecker {
    /* Terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("z-aligned") {
    new Level1 {
      val block = Block(Pos(2,2),Pos(2,2)) // 2,2

      assert(Pos(2,2).deltaRow(-2) ==  Pos(0,2))
      assert(Pos(2,2).deltaCol(-2) ==  Pos(2,0))

      // Preserve row value
      assert(block.right == Block(Pos(2,3),Pos(2,4)))
      assert(block.left == Block(Pos(2,0),Pos(2,1)))

      // Preserve column value
      assert(block.up == Block(Pos(0,2), Pos(1,2)))
      assert(block.down == Block(Pos(3,2), Pos(4,2)))
    }
  }

  test("x-aligned") {
    new Level1 {
      val block = Block(Pos(2,1), Pos(2,2))   // (2,1) -> (2,2)

      // -Preserve row value
      assert( block.right ==  Block(Pos(2,3),Pos(2,3)))
      assert( block.left  ==  Block( Pos(2,0), Pos(2,0)))

      // -Preserve column value
      assert(block.up == Block(Pos(1,1), Pos(1,2)))
      assert(block.down == Block(Pos(3,1), Pos(3,2)))

    }
  }


  test("y-aligned") {
    new Level1 {
      val block = Block(Pos(1,2), Pos(2,2))   // (2,1) -> (2,2)
      // Preserve row value 
      assert( block.right == Block(Pos(1,3),Pos(2,3)))
      assert( block.left ==  Block(Pos(1,1),Pos(2,1)))

      // Preserve column value
      assert(block.up == Block(Pos(0,2), Pos(0,2)))
      assert(block.down == Block(Pos(3,2), Pos(3,2)))
    }
  }



  test("Find all legal neighbors.") {
    new Level1 {
      val block = startBlock
      val all   = block.neighbors     
      val neighbors = block.legalNeighbors     
      assert(neighbors.size == 2)      
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


  /**
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  */
}

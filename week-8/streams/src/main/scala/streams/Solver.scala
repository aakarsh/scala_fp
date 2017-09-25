package streams

import common._

/**
  * This component implements the solver for the Bloxorz game
  */
trait Solver extends GameDef {


  type BlockStream = Stream[(Block, List[Move])]

  /**
    * Returns `true` if the block `b` is at the final position
    */
  def done(b: Block): Boolean =
    b.isStanding && (b match {
      case Block(b1: Pos, b2:Pos) => (b1 == goal && b2 == goal)
      case _ => false
    })

  /**
    * This function takes two arguments: the current block `b` and
    * a list of moves `history` that was required to reach the
    * position of `b`.
    *
    * The `head` element of the `history` list is the latest move
    * that was executed, i.e. the last move that was performed for
    * the block to end up at position `b`.
    *
    * The function returns a stream of pairs: the first element of
    * each pair is a neighboring block, and the second element
    * is the augmented history of moves required to reach this block.
    *
    * It should only return valid neighbors, i.e. block positions
    * that are inside the terrain.
    */
  def neighborsWithHistory(b: Block, history: List[Move]): BlockStream =
    (b.up,Up::history)       #::
    (b.down,Down::history)   #::
    (b.left,Left::history)   #::
    (b.right,Right::history) #:: Stream.empty

  /**
    * This function returns the list of neighbors without the block
    * positions that have already been explored. We will use it to
    * make sure that we don't explore circular paths.
    */
  def filterBySet(neighbors: BlockStream, explored: Set[Block]): BlockStream =
    neighbors.filter({ case (b:Block, _ ) => explored.contains(b)})


  /**
    * The function `from` returns the stream of all possible paths
    * that can be followed, starting at the `head` of the `initial`
    * stream.
    *
    * The blocks in the stream `initial` are sorted by ascending path
    * length: the block positions with the shortest paths (length of
    * move list) are at the head of the stream.
    *
    * The parameter `explored` is a set of block positions that have
    * been visited before, on the path to any of the blocks in the
    * stream `initial`. When search reaches a block that has already
    * been explored before, that position should not be included a
    * second time to avoid cycles.
    *
    * The resulting stream should be sorted by ascending(increasing order,
    * shortest path first) path length, i.e. the block positions that can
    * be reached with the fewest amount of moves should appear first in
    * the stream.
    *
    * Note: the solution should not look at or compare the lengths of
    * different paths - the implementation should naturally construct
    * the correctly sorted stream.
    *
    */
  def from(initial : BlockStream,  // Blocks by ascending path length,
                                                   // Along with list of moves to reach that block
                                                   // Nearest blocks likely to appear first.

           explored : Set[Block]) :                // All blocks which have already been
                                                   // explored and should not be revisited.

      BlockStream  = {             // All possible paths that can be reachable from
                                                   // initial.head

        // explored will not get updated here since the only node that gets really explored
        // is initial.head.

        // initial can be the the peripheral block list.

    /**
     * The heads of the initial define the periphary of
     * exploration. The explored defines that which has
     * already been explored.
     */
    initial match {

      // if there is no head there are no paths
      case Stream.Empty => Stream.empty

      // Take the head - Find all possible blocks reachable from the head.
      //
      // For every block reachable from head construct a list of all
      // blocks reachable from this block ensure that list of blocks
      // explored contains this.

      case (block:Block, history:List[Move]) #:: xs     =>        {

        // Construct a list of neighbors we have not visited,
        // that are reachable from the current head
        //
        // Consume the head block and generate a stream of new blocks reachable from head block

        val newBorder: BlockStream = filterBySet(neighborsWithHistory(block, history), explored)

        /**
         * From must return all blocks reachable from the head in
         * ascending order.  clearly the newBorder blocks are the most
         * reachable blocks from the head.  so they must be at the
         * head of the stream.  after the new border the next most
         * reachble will be.
         */
        def toSet(blockStream: BlockStream):Set[Block] =
          blockStream.map({ case (b:Block,_) => b }).toSet

        newBorder #::: from(newBorder, toSet(newBorder) ++ explored)
      }
    }
  }

  /**
    * The stream of all paths that begin at the starting block.
    */
  lazy val pathsFromStart: BlockStream = {
    val initalStream =  (startBlock,List[Move]()) #::Stream.empty
    val initalExplored = Set(startBlock) // start by exploring first block

    from(initalStream, initalExplored)
  }

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal : BlockStream = {
    val endInGoal = pathsFromStart.filter({case (b:Block,_) => done(b)})
    endInGoal
  }

  /**
    * The (or one of the) shortest sequence(s) of moves to reach the
    * goal. If the goal cannot be reached, the empty list is returned.
    *
    * Note: the `head` element of the returned list should represent
    * the first move that the player should perform from the starting
    * position.
    */
  lazy val solution: List[Move] =  pathsToGoal match {
    case Stream.Empty => List[Move]()
    case ( _, moves: List[Move]) #:: xs => moves  // Assuming that head is shortest path to the goal
  }

}

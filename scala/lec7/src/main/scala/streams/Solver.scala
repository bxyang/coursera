package streams

import common._
import sun.org.mozilla.javascript.internal.ast.Yield

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = {
    println(b.b1 + " " + b.b2 + " " + goal)
    ((b.b1.x == goal.x) && (b.b1.y == goal.y) && (b.b2.x == goal.x) && (b.b2.y == goal.y))
    
  }

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
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   * 
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
	  val n = b.legalNeighbors
	  if (n.isEmpty) Stream.empty[(Block, List[Move])]
	  else n.foldLeft(Stream.empty[(Block, List[Move])])(
	      (ret, item) => Stream.cons((item._1, item._2::history), ret)
	      )
  }

  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    neighbors.filterNot((item: (Block, List[Move])) => explored.contains(item._1))
  }

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
   * The resulting stream should be sorted by ascending path length,
   * i.e. the block positions that can be reached with the fewest
   * amount of moves should appear first in the stream.
   * 
   * Note: the solution should not look at or compare the lengths
   * of different paths - the implementation should naturally
   * construct the correctly sorted stream.
   */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    println("initial: " + initial.length + " explored " + explored.size)
    if (initial.isEmpty) initial
    else {
      val ret = initial.foldLeft(Stream.empty[(Block, List[Move])])((neighbor, item) => neighbor ++ newNeighborsOnly(neighborsWithHistory(item._1, item._2), explored))
      println("ret len = " + ret.length)
      if (ret.filter(item => done(item._1)).isEmpty) from(ret, ret.toList.foldLeft(Set.empty[Block])((s, item) => s+item._1) ++ explored)
      else ret
    }
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream.cons((startBlock, List.empty[Move]), Stream.empty[(Block, List[Move])]), Set.empty[Block]+startBlock)

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = pathsFromStart.filter(elem => done(elem._1))

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = if (pathsToGoal == Stream.Empty) List.empty[Move] 
		  						 else pathsToGoal.foldLeft(pathsToGoal.head._2)((ret, item) => if (item._2.length < ret.length) item._2 else ret)
}
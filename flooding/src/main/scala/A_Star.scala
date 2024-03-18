import scala.collection.mutable.PriorityQueue
import scala.annotation.{tailrec, unused}
import scala.collection.mutable

/** A state class for the A* algorithm
 *
 * @param graph   the graph that the sequence of actions has resulted in
 * @param path    the list of actions taken thus far (in reverse order)
 * @param h_value the heuristic value of the graph
 * @author Evan Dreher
 */
@unused case class State(graph: Graph, path: List[Byte], h_value: Int) {
  /** Finds all possible actions that can be taken, and performs them on the graph generating a
   * new state for each action
   *
   * @param heuristic the heuristic being used by the A* search algorithm
   * @return A list of all successor states
   */
  def get_successors(heuristic: Graph => Int): List[State] = {
    val actions = graph.adjacency(0).map(graph.labels(_).toByte).distinct
    actions.map(action => State(graph pick action, action :: path, heuristic(graph pick action)))
  }
}

/** The standard A* search algorithm but without the closed list or queue checking since
 * it is computationally inefficient to compute things for this particular problem
 * due to the rarity of it occurring.
 *
 * @param graph     The starting graph
 * @param heuristic The heuristic for A* to make choices based on
 * @return An ordered pair of the number of nodes explored, and the optimal path found by the algorithm
 * @author Evan Dreher
 */
def A_star(graph: Graph, verbose: Int = 0)(heuristic: Graph => Int): (Long, List[Byte]) = {
  var nodes_explored: Long = 0L
  val open: mutable.PriorityQueue[State] = mutable.PriorityQueue()(
    Ordering.by((s: State) => s.path.length + s.h_value).reverse
  )
  open.enqueue(State(graph, List.empty, heuristic(graph)))
  while (open.nonEmpty) {
    // get next state graph and return path if its a goal state
    val current = open.dequeue()
    if verbose > 0 then println(s"Dequeued: \n${current.path}")
    nodes_explored = nodes_explored + 1
    if current.graph.isGoal then {
      if verbose > 0 then println(s"Final fringe size: ${open.size}")
      return (nodes_explored, current.path.reverse)
    }
    val successors = current.get_successors(heuristic)
    if verbose > 0 then println(s"Queueing ${successors.length} successors")
    // enqueue all its successors if its not
    open.enqueue(successors: _*)
    if verbose > 1 then println(s"Current fringe size: ${open.size}")
  }
  // default value that should never be returned but if I don't have it scala yells at me
  (Integer.MAX_VALUE, List())
}

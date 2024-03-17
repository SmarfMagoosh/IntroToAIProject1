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

/** A more memory efficient state class for the A* algorithm
 *
 * @param parent  the state that his state is a child of
 * @param action  the most recent action taken
 * @param h_value the heuristic value of the graph resultant of performing all the actions on the initial graph
 * @author Evan Dreher
 */
@unused case class State2(parent: Option[State2], action: Byte, h_value: Int) {
  /** Finds all possible actions that can be taken, and performs them on the graph generating a
   * new state for each action
   *
   * @param heuristic the heuristic being used by the A* search algorithm
   * @return A list of all successor states
   */
  def get_successors(heuristic: Graph => Int)(graph: Graph): List[State2] = {
    val g = g_prime(graph)
    val actions = g.adjacency(0).map(g.labels(_).toByte).distinct
    actions.map(action => State2(Some(this), action, heuristic(g pick action)))
  }

  /** Gets the list of actions taken to get to this state by performing a tree traversal using
   * the parent references
   *
   * @param path keeps track of which actions were taken as we traverse the tree
   * @return the list of actions to get to this state
   * @author Evan Dreher
   */
  @tailrec final def get_path(path: List[Byte] = List.empty): List[Byte] = {
    if parent.isEmpty then path else parent.get.get_path(action :: path)
  }

  /** Gets the current state of the graph base on actions taken
   *
   * @param g the starting graph we perform actions on
   * @return the result of performing all actions taken thus far on g
   */
  def g_prime(g: Graph): Graph = get_path().foldLeft(g)(_ pick _)
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
    if current.graph.isGoal then return (nodes_explored, current.path.reverse)
    val successors = current.get_successors(heuristic)
    if verbose > 0 then println(s"Queueing ${successors.length} successors")
    // enqueue all its successors if its not
    open.enqueue(successors: _*)
    if verbose > 1 then println(s"Current fringe size: ${open.size}")
  }
  // default value that should never be returned but if I don't have it scala yells at me
  (Integer.MAX_VALUE, List())
}

/** A slower but more memory efficient version of the A* algorithm. Rather than keeping track of
 * every graph we've come across, we simply keep track of the actions we've taken thus far
 * and compute the graph starting with the original when we need successors of a state
 *
 * @param graph     The starting graph
 * @param verbose   0 for no output, 1 dequeueing and queueing, 2 for fringe size
 * @param heuristic The heuristic for A* to make choices based on
 * @return An ordered pair of the number of nodes explored, and the optimal path found by the algorithm
 * @author Evan Dreher
 */
def A_star_2(graph: Graph, verbose: Int = 0)(heuristic: Graph => Int): (Long, List[Byte]) = {
  var nodes_explored: Long = 0L
  val open: mutable.PriorityQueue[State2] = mutable.PriorityQueue()(
    Ordering.by((s: State2) => s.get_path().length + s.h_value).reverse
  )
  open.enqueue(State2(None, -1, heuristic(graph)))
  while (open.nonEmpty) {
    // get next state graph and return it if its a goal
    val current = open.dequeue()
    if verbose > 0 then println(s"Dequeued: \n${current.get_path()}")
    val current_graph = current.g_prime(graph)
    nodes_explored = nodes_explored + 1
    if current_graph.isGoal then return (nodes_explored, current.get_path())

    // if its not a goal, enqueue all its successors
    val successors = current.get_successors(heuristic)(graph)
    if verbose > 0 then println(s"Queueing ${successors.length} successors")
    open.enqueue(successors: _*)
    if verbose > 1 then println(s"Current fringe size: ${open.size}")
  }
  // default value that should never be returned but if I don't have it scala yells at me
  (Integer.MAX_VALUE, List())
}

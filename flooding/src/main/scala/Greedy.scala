import scala.annotation.tailrec

/** An abstracted implementation for a greedy approach to the problem to compare results to
 *
 * @param compare   decides with lower valued or higher valued states are optimal
 * @param heuristic the heuristic for evaluating a state
 * @param graph     the initial graph
 * @return A goal state including the path found to it
 * @author Evan Dreher
 */
def greedy_search(compare: (State, State) => Boolean)(heuristic: Graph => Int)(graph: Graph): State = {
  @tailrec def greedy(state: State = State(graph, List.empty, heuristic(graph))): State = {
    if state.graph.isGoal then state else greedy(state.get_successors(heuristic).reduce(
      (s1, s2) => if compare(s1, s2) then s1 else s2
    ))
  }

  greedy()
}

/** A greedy algorithm always picking the highest valued neighbor.
 * uses the "most vertices connected to blob" heuristic
 *
 * @param g the starting graph
 * @return A goal state including path found to it
 * @author Evan Dreher
 */
def max_greedy(g: Graph): State = greedy_search(_.h_value > _.h_value)(g_h1)(g)

/** A greedy algorithm always picking the lowest valued neighbor.
 * uses the "absorb most vertices into blob" heuristic
 *
 * @param g the starting graph
 * @return A goal state including path found to it
 * @author Evan Dreher
 */
def min_greedy(g: Graph): State = greedy_search(_.h_value < _.h_value)(g_h2)(g)

/** An improved greedy strategy. Tries to do the first greedy strategy (maximizing degree of the blob)
 * but if that doesn't improve it enough, fall back to the second greedy strategy (absorb as many nodes
 * as possible)
 *
 * @param graph the starting graph
 * @return A solution state including path found
 * @author Evan Dreher
 */
def improved_greedy(graph: Graph): State = {
  @tailrec def greedy(state: State = State(graph, List.empty, 0)): State = {
    // if complete
    if state.graph.isGoal then state

    // if its the first iteration use touch most nodes heuristic
    else if state.path.isEmpty then greedy(state.get_successors(g_h2).reduce(
      (s1, s2) => if s1.h_value > s2.h_value then s1 else s2
    ))
    // the part where greedy is actually smart
    else {
      val max_successors = state.get_successors(g_h2)
      val improvement = max_successors.foldLeft(0)(_ max _.h_value) - state.h_value
      if improvement <= 2 then greedy(state.get_successors(g_h1).reduce(
        (s1, s2) => if s1.h_value < s2.h_value then s1 else s2
      ))
      else greedy(max_successors.reduce(
        (s1, s2) => if s1.h_value > s2.h_value then s1 else s2
      ))
    }
  }
  greedy()
}
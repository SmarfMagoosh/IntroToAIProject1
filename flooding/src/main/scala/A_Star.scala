import java.util.PriorityQueue


case class State(graph: Graph, path: List[Int], h_value: Int) {
  def get_successors(heuristic: Graph => Int): List[State] = {
    val actions = graph.adjacency(0).map(graph.labels(_)).distinct
    actions.map(color => {
      val g_prime = graph pick color
      State(g_prime, color :: path, heuristic(g_prime))
    })
  }
  def f: Int = path.length + h_value
  override def toString: String = s"Path: ${path.mkString("[", ", ", "]")}\nHeuristic:$h_value\nGraph:\n$graph\n"
}

def A_star(graph: Graph)(heuristic: Graph => Int): (Int, List[Int]) = {
  var nodes_explored = 0
  val open: PriorityQueue[State] = new PriorityQueue(Ordering.by((s: State) => s.path.length + s.h_value).reverse)
  open.add(State(graph, List(), heuristic(graph)))
  while (!open.isEmpty) {
    val current = open.poll()
    nodes_explored = nodes_explored + 1
    if current.graph.isGoal then return (nodes_explored, current.path.reverse)
    current.get_successors(heuristic).foreach(open.add)
  }
  (Integer.MAX_VALUE, List())
}

import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.collection.mutable

case class State(graph: Graph, path: List[Int], h_value: Int) {
  def get_successors(heuristic: Graph => Int): List[State] = {
    val actions = graph.adjacency(0).map(graph.labels(_)).distinct
    actions.map(action => State(graph pick action, action :: path, heuristic(graph pick action)))
  }
}

case class State2(parent: Option[State2], action: Int, h_value: Int) {
  def get_successors(heuristic: Graph => Int)(graph: Graph): List[State2] = {
    val g = g_prime(graph)
    val actions = g.adjacency(0).map(g.labels(_)).distinct
    actions.map(action => State2(Some(this), action, heuristic(g pick action)))
  }

  @tailrec final def get_path(path: List[Int] = List.empty): List[Int] = {
    if parent.isEmpty then path else parent.get.get_path(action :: path)
  }

  def g_prime(g: Graph): Graph = get_path().foldLeft(g)(_ pick _)
}

def A_star(graph: Graph)(heuristic: Graph => Int): (Int, List[Int]) = {
  var nodes_explored = 0
  val open: mutable.PriorityQueue[State] = mutable.PriorityQueue()(
    Ordering.by((s: State) => s.path.length + s.h_value).reverse
  )
  open.enqueue(State(graph, List.empty, heuristic(graph)))
  while (open.nonEmpty) {
    val current = open.dequeue()
    nodes_explored = nodes_explored + 1
    if current.graph.isGoal then return (nodes_explored, current.path.reverse)
    open.enqueue(current.get_successors(heuristic): _*)
  }
  (Integer.MAX_VALUE, List())
}

def A_star_2(graph: Graph)(heuristic: Graph => Int): (Int, List[Int]) = {
  var nodes_explored = 0
  val open: mutable.PriorityQueue[State2] = mutable.PriorityQueue()(
    Ordering.by((s: State2) => s.get_path().length + s.h_value).reverse
  )
  open.enqueue(State2(None, -1, heuristic(graph)))
  while (open.nonEmpty) {
    val current = open.dequeue()
    val current_graph = current.g_prime(graph)
    nodes_explored = nodes_explored + 1
    if current_graph.isGoal then return (nodes_explored, current.get_path())
    open.enqueue(current.get_successors(heuristic)(graph): _*)
  }
  (Integer.MAX_VALUE, List())
}

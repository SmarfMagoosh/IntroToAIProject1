import scala.collection.mutable.ListBuffer as MutList

/**
 * A standard undirected graph with labellable vertices
 * @param vertices a range of integers indicating
 * @param edges ordered pairs of integers indicating undirected edges of the graph
 */
class Graph(private val vertices: Range, private val edges: List[(Int, Int)]) {
  // current color labels of the graph
  val labels: Array[Int] = vertices.map(_ => 0).toArray

  // adjacency list of the graph (array used for constant lookups)
  val adjacency: Array[List[Int]] = {
    val adjList: Array[MutList[Int]] = labels.map(_ => MutList())
    for edge <- edges do {
      adjList(edge._1).addOne(edge._2)
      adjList(edge._2).addOne(edge._1)
    }
    adjList.map(_.toList)
  }

  // TODO: add extra methods here because we need em fr fr

  override def toString: String =
    s"${vertices.map(v => s"$v (${labels(v)}) -> ${adjacency(v).mkString("[", ", ", "]")}").mkString("\n")}"
}
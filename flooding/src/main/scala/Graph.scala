import scala.collection.mutable.ListBuffer as MutList
import scala.collection.mutable.Queue as MutQueue
import scala.annotation.tailrec

/** A standard undirected graph with labellable vertices
  * @param vertices a range of integers indicating
  * @param edges ordered pairs of integers indicating undirected edges of the graph
  */
case class Graph(vertices: Range, edges: List[(Int, Int)]) {
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

  /** @param newColor
    *   the color to change the vertex to
    * @param vertex
    *   the vertex to start the coloring from
    * @return
    *   a new Graph containing the resulting state after applying the operation
    */
  def pick(newColor: Int, vertex: Int = 0): Graph = {
    val currentColor = labels(vertex)
    // don't do anything and return a copy if the color is the same
    if currentColor == newColor then return this.copy()

    // keep track of the vertices we've checked
    val checked: Array[Boolean] = vertices.map(_ => false).toArray
    checked(vertex) = true

    @tailrec
    def verticesToColor(
        q: List[Int],
        acc: List[Int] = List()
    ): List[Int] = q match {
      case Nil => acc
      case h :: tail =>
        if labels(h) == currentColor then
          // getting newQ and updating checked are "constant time" bcz a vertex can have a max of 4 neighbors and we're prepending to tail
          val newQ = adjacency(h).filter(v => !checked(v)) ::: tail
          for v <- adjacency(h) do checked(v) = true
          verticesToColor(newQ, h :: acc)
        else verticesToColor(tail, acc)
    }
    val toColor = verticesToColor(List(vertex))

    // TODO: combine all vertices in the "blob"

    // return a new graph with the resulting state
    val newGraph = this.copy()
    for v <- toColor do newGraph.labels(v) = newColor
    newGraph
  }

  override def toString: String =
    s"${vertices.map(v => s"$v (${labels(v)}) -> ${adjacency(v).mkString("[", ", ", "]")}").mkString("\n")}"
}

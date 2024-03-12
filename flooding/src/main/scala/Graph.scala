import scala.collection.mutable.ListBuffer as MutList
import scala.annotation.tailrec

/** A standard undirected graph with labelled vertices
 *
 * @param vertices a range of integers indicating the number of vertices on the graph
 * @param edges    ordered pairs of integers indicating undirected edges of the graph
 * @author Evan Dreher, Micah Nicodemus
 */
case class Graph(vertices: Range, edges: List[(Int, Int)]) {

  /**
   * Array to keep track of the labels of the vertices
   */
  val labels: Array[Int] = vertices.map(_ => 0).toArray

  /**
   * Adjacency List for the Graph
   */
  val adjacency: Array[List[Int]] = {
    val adjList: Array[MutList[Int]] = labels.map(_ => MutList())
    for edge <- edges do {
      adjList(edge._1).addOne(edge._2)
      adjList(edge._2).addOne(edge._1)
    }
    adjList.map(_.toList)
  }

  /**
   * true if this graph is a goal state (has only 1 vertex)
   */
  val isGoal: Boolean = this.vertices.end == 1

  /** Recolors a vertex to be a new color and blobifies the resulting graph so
   * that no neighbors have the same color
   *
   * @param newColor the color to change the vertex to
   * @return a new Graph containing the resulting state after applying the operation
   * @author Micah Nicodemus
   */
  def pick(newColor: Int): Graph = {
    // don't do anything and return a copy if the color is the same
    if labels.head == newColor then return Graph(vertices, edges)

    // vertices in and out of the blob
    val verticesToRemove = adjacency.head.filter(v => labels(v) == newColor)
    val verticesToRemain = vertices.filterNot(verticesToRemove contains _)

    // vertex in G => vertex in G'
    val vertexMap = verticesToRemain.zipWithIndex.toMap
    val blobIndex = vertexMap(0)

    // maps edges in G to edges in G'
    val newEdges = (for {
      edge <- edges
      v1Prime = vertexMap.getOrElse(edge._1, blobIndex)
      v2Prime = vertexMap.getOrElse(edge._2, blobIndex)
      if v1Prime != v2Prime
    } yield {
      (v1Prime min v2Prime, v1Prime max v2Prime)
    }).distinct

    // create G' and label its vertices
    val newG = Graph(verticesToRemain.indices, newEdges)
    for v <- verticesToRemain do newG.labels(vertexMap(v)) = labels(v)
    newG.labels(vertexMap(0)) = newColor
    newG
  }

  override def toString: String =
    s"${vertices.map(v => s"$v (${labels(v)}) -> ${adjacency(v).mkString("[", ", ", "]")}").mkString("\n")}"
}

import scala.collection.mutable.ListBuffer as MutList
import scala.collection.mutable.Queue as MutQueue
import scala.annotation.tailrec
import scala.collection.mutable.Map as MutMap

/** A standard undirected graph with labelled vertices
  *
  * @param vertices
  *   a range of integers indicating the number of vertices on the graph
  * @param edges
  *   ordered pairs of integers indicating undirected edges of the graph
  * @author
  *   Evan Dreher, Micah Nicodemus
  */
case class Graph(vertices: Range, edges: List[(Int, Int)]) {

  /** Array to keep track of the labels for our graph, indices relate to
    * vertices
    */
  val labels: Array[Int] = vertices.map(_ => 0).toArray

  /** Adjacency List for the Graph. Indices of the array correspond to vertices.
    * Values in the list correspond to vertices it points to.
    */
  val adjacency: Array[List[Int]] = {
    val adjList: Array[MutList[Int]] = labels.map(_ => MutList())
    for edge <- edges do {
      adjList(edge._1).addOne(edge._2)
      adjList(edge._2).addOne(edge._1)
    }
    adjList.map(_.toList)
  }

  /** TODO: insert description so our code is OP
    *
    * @param newColor
    *   the color to change the vertex to
    * @param vertex
    *   the vertex to start the coloring from
    * @return
    *   a new Graph containing the resulting state after applying the operation
    */
  def pick(newColor: Int, vertex: Int = 0): Graph = {
    // don't do anything and return a copy if the color is the same
    if labels(vertex) == newColor then return this.copy()

    val verticesToRemove =
      adjacency(vertex).filter(v => labels(v) == newColor)
    val verticesToRemain = vertices.diff(verticesToRemove).toList
    val verticesMapping =
      MutMap[Int, Int](verticesToRemain.zipWithIndex.map((v, i) => v -> i): _*)
    for v <- verticesToRemove do verticesMapping += v -> verticesMapping(vertex)

    val newEdges = edges
      .map((v1, v2) => {
        val v1Mapped = verticesMapping(v1)
        val v2Mapped = verticesMapping(v2)
        (v1Mapped.min(v2Mapped), v1Mapped.max(v2Mapped))
      })
      .filter((v1, v2) => v1 != v2)
      .distinct

    val newG = Graph(0 until verticesToRemain.length, newEdges)
    for v <- verticesToRemain do newG.labels(verticesMapping(v)) = labels(v)
    newG.labels(verticesMapping(vertex)) = newColor
    newG
  }

  override def toString: String =
    s"${vertices.map(v => s"$v (${labels(v)}) -> ${adjacency(v).mkString("[", ", ", "]")}").mkString("\n")}"
}

import scala.collection.mutable
import scala.collection.mutable.Queue

/** Longest shortest path heuristic for A*
 *
 * @param g the graph the heuristic is evaluating
 * @return the length of the longest shortest path on the graph
 * @author Evan Dreher
 */
def h1(g: Graph): Int = {
  // initialize single source as the blob
  val found_distances: Array[Int] = g.vertices.map(v => if v == 0 then 0 else Integer.MAX_VALUE).toArray

  // performs bfs on the graph labelling vertices as the distance at which we found them as we go
  val q = mutable.Queue[Int](0)
  while (q.nonEmpty) {
    val c: Int = q.dequeue()
    g.adjacency(c).foreach(v => {
      if found_distances(v) == Integer.MAX_VALUE then {
        found_distances(v) = found_distances(c) + 1
        q.enqueue(v)
      }
    })
  }

  // return longest shortest path
  found_distances.max
}

/** Densest layer heuristic. uses the following procedure
 * First group vertices by their distance from the blob. For each distance, the layer density equals
 * the number of unique colors at distance plus the distance minus 1. Intuitively, this is the minimum
 * possible number of moves required to absorb all vertices at that distance. We return the highest
 * layer density.
 *
 * @param g The graph to evaluate
 * @return the highest layer density on the graph.
 * @author Evan Dreher
 */
def h2(g: Graph): Int = {
  // initialize the single source as the blob
  val found_distances: Array[Int] = g.vertices.map(v => if v == 0 then 0 else Integer.MAX_VALUE).toArray

  // performs bfs on the graph labelling vertices as the distance at which we found them as we go
  val q = mutable.Queue[Int](0)
  while (q.nonEmpty) {
    val c: Int = q.dequeue()
    g.adjacency(c).foreach(v => {
      if found_distances(v) == Integer.MAX_VALUE then {
        found_distances(v) = found_distances(c) + 1
        q.enqueue(v)
      }
    })
  }

  // group vertices by their distance
  val all_paths = found_distances.zipWithIndex.groupBy((d, _) => d)

  // maps each distance to its layer density
  val paths_with_colors = all_paths.transform((_, v) => v.distinctBy((_, y) => g.labels(y)).length)

  // finds the largest layer density
  paths_with_colors.foldLeft(-1)((c: Int, p: (Int, Int)) => c max (p._1 + p._2 - 1))
}
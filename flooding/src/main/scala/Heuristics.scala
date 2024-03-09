import scala.collection.mutable
import scala.collection.mutable.Queue

def h1(g: Graph): Int = {
  val found_distances: Array[Int] = g.vertices.map(v => if v == 0 then 0 else Integer.MAX_VALUE).toArray
  val q = mutable.Queue[Int](0)
  while(q.nonEmpty) {
    val c: Int = q.dequeue()
    g.adjacency(c).foreach(v => {
      if found_distances(v) == Integer.MAX_VALUE then {
        found_distances(v) = found_distances(c) + 1
        q.enqueue(v)
      }
    })
  }
  found_distances.max
}

def h2(g: Graph): Int = {
  val found_distances: Array[Int] = g.vertices.map(v => if v == 0 then 0 else Integer.MAX_VALUE).toArray
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
  val all_paths = found_distances.zipWithIndex.groupBy((d, _) => d)
  val paths_with_colors = all_paths.transform((k, v) => v.distinctBy((x, y) => g.labels(y)).length)
  //paths_with_colors.map((d, c) => d + c - 1).max
  paths_with_colors.foldLeft(-1)((c: Int, p: (Int, Int)) => c max (p._1 + p._2 - 1))
}

def h3(g: Graph): Int = 0 // bfs

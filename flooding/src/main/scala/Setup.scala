import scala.util.Random
import java.io.File
import java.util.Scanner
import java.io.PrintWriter
import scala.annotation.unused
import scala.collection.mutable.Map as MutMap

/** Generates a random grid of colored nodes.
  *
  * @param rows
  *   the number of rows for the graph
  * @param cols
  *   the number of columns for the graph
  * @param colors
  *   the number of colors each vertex on the graph could be
  * @return
  *   a row by col grid of random numbers ranging from 0 to colors (inclusive)
  * @author
  *   Evan Dreher
  */
@unused def pre_graph(rows: Int, cols: Int, colors: Int): Seq[Seq[Int]] = {
  (0 until rows).map(_ => (0 until cols).map(_ => Random.nextInt(colors)))
}

/** Reads a grid of labels from a file. The first number in the file is the
  * number of rows then the number of columns the rest of the file is the
  * color_id for each respective node. This method utilizes java.scanner.nextInt
  * so any separation of the numbers will suffice.
  *
  * @param name
  *   the path to the file with the specified graph
  * @return
  *   A 2D grid of colors read from a file in "./samples
  * @author
  *   Evan Dreher
  */
@unused def pre_graph_from_file(name: String): Seq[Seq[Int]] = {
  val scantron: Scanner = new Scanner(new File(s"./samples/$name"))
  val (rows, cols) = (scantron.nextInt, scantron.nextInt)
  val ret =
    for _ <- 0 until rows yield for _ <- 0 until cols yield scantron.nextInt
  scantron.close()
  ret
}

/** Turns the unconnected 2D seq into a proper graph
  *
  * @param pre_graph
  *   The 2D sequence to turn into a labelled graph
  * @return
  *   A labelled graph corresponding to the input grid of colors
  * @author
  *   Evan Dreher
  */
@unused def graphify_pre_graph(pre_graph: Seq[Seq[Int]]): Graph = {
  val (rows, cols) = (pre_graph.length, pre_graph.head.length)
  val flat = pre_graph.flatten
  def get_edges(index: Int): List[(Int, Int)] = {
    val lastRow = index >= (rows * cols) - cols
    val lastCol = index % cols == (cols - 1)
    (lastCol, lastRow) match
      case (true, true)  => List()
      case (true, false) => List((index, index + cols))
      case (false, true) => List((index, index + 1))
      case _             => List((index, index + cols), (index, index + 1))
  }
  val vertices = flat.indices
  val g: Graph = Graph(vertices, (vertices flatMap get_edges).toList)
  vertices.foreach(i => g.labels(i) = flat(i))
  g
}

/** Simplifies a grid graph by merging neighbors with equal vertices into one
  * vertex while maintaining edges in the original graph.
  *
  * @param g
  *   A grid graph with randomly colored vertices
  * @return
  *   A graph that is color-wise identical to the input graph but with
  *   neighboring vertices of the same color clustered together as 1 Vertex
  * @author
  *   Evan Dreher
  */
@unused def blobify(g: Graph): Graph = {
  val blobs: MutMap[Int, Int] = MutMap()
  def dfs(v: Int): Array[Boolean] = {
    val in_blob: Array[Boolean] = g.labels.map(_ => false)
    def recurse(current: Int = v): Unit = {
      in_blob(current) = true
      g.adjacency(current)
        .filter(n => g.labels(n) == g.labels(current) && !in_blob(n))
        .foreach(recurse)
    }
    recurse()
    in_blob
  }
  var next_blob: Int = 0
  for v <- g.vertices do if !(blobs contains v) then {
    val blob: Array[Boolean] = dfs(v)
    for b <- blob.indices; if blob(b) do blobs.put(b, next_blob)
    next_blob = next_blob + 1
  }
  val edges = (for edge <- g.edges; if edge._1 != edge._2 yield {
    val to_prime = blobs(edge._1)
    val from_prime = blobs(edge._2)
    if to_prime > from_prime then (from_prime, to_prime) else (from_prime, to_prime)
  }).distinct

  val g_prime: Graph = Graph(0 to blobs.values.max, edges)
  for v <- g.vertices do g_prime.labels(blobs(v)) = g.labels(v)
  g_prime
}

/** Sends a graph of your choice to the shadow realm (SSD) as a .txt file so it
  * can be read later.
  *
  * @param g
  *   The graph to be sent away.
  * @param name
  *   The name of the file you want to save the graph to.
  * @author
  *   Evan Dreher
  */
@unused def send_graph_to_shadow_realm(g: Graph, name: String): Unit = {
  val f: File = new File(s"./samples/$name")
  val pw: PrintWriter = new PrintWriter(f)
  pw.write(s"${g.vertices.end + 1} ${g.edges.length}\n")
  pw.write(s"${g.edges.mkString("", "\n", "")}\n")
  pw.write(s"${g.labels.mkString("", " ", "")}")
  pw.flush()
}

/** Retrieves a graph from the shadow realm (SSD) by reading it from the
  * specified file path in ./samples
  *
  * @param name
  *   The name of the file at which the graph is stored
  * @return
  *   The graph stored at the file
  * @author
  *   Evan Dreher
  */
@unused def retrieve_graph_from_shadow_realm(name: String): Graph = {
  val scantron: Scanner = new Scanner(new File(s"./samples/$name"))
  def readInt: Int = scantron.nextInt
  val (num_vertices, num_edges) = (readInt, readInt)
  val edges: List[(Int, Int)] =
    (1 to num_edges).map(_ => (readInt, readInt)).toList
  val labels: Array[Int] = (1 to num_vertices).map(_ => readInt).toArray
  val g: Graph = Graph(0 until num_vertices, edges)
  for i <- labels.indices do g.labels(i) = labels(i)
  g
}

def time_it[A](fn: => A): Long = {
  val start = System.currentTimeMillis()
  fn
  System.currentTimeMillis() - start
}

@main def main(): Unit = {
//  val graphs: List[Graph] = (5 to 200 by 5).map(x => graphify_pre_graph(pre_graph(x, x, x / 5))).toList
//  val pw: PrintWriter = new PrintWriter(new File("runtime.csv"))
//  for graph <- graphs do {
//    val time = time_it(blobify(graph))
//    pw.write(s"${graph.vertices.end},$time\n")
//    println(s"${Math.sqrt(graph.vertices.end)}")
//  }
//  pw.close()
  val g: Graph = Graph(0 to 5, List((0, 1), (0, 2), (0, 3), (1, 2), (3, 4), (4, 5)))
  g.labels(0) = 1
  g.labels(1) = 3
  g.labels(2) = 2
  g.labels(3) = 2
  g.labels(4) = 1
  g.labels(5) = 3
  val g_prime = g.pick(2)
  println(g_prime)
}

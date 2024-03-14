import scala.util.Random
import java.io.File
import java.util.Scanner
import java.io.PrintWriter
import scala.annotation.unused
import scala.collection.mutable.Map as MutMap

/** Generates a 2D random grid of numbers whose range is [0, colors).
 *
 * @param rows   the number of rows for the graph
 * @param cols   the number of columns for the graph
 * @param colors the number of colors each vertex on the graph could be
 * @return a grid of random numbers in the range [0 to colors)
 * @author Evan Dreher
 */
@unused def pre_graph(rows: Int, cols: Int, colors: Int): Seq[Seq[Int]] = {
  (0 until rows).map(_ => (0 until cols).map(_ => Random.nextInt(colors)))
}

/** Reads a grid of labels from a file. The first number in the file is the
 * number of rows then the number of columns. The rest of the file is the
 * color value for each respective node.
 *
 * @param name The path to the file with the specified graph
 * @return A 2D grid of colors read from a file in "./samples
 * @author Evan Dreher
 */
@unused def pre_graph_from_file(name: String): Seq[Seq[Int]] = {
  val scantron: Scanner = new Scanner(new File(s"./samples/$name"))
  val (rows, cols) = (scantron.nextInt, scantron.nextInt)
  val ret = for _ <- 0 until rows yield for _ <- 0 until cols yield scantron.nextInt
  scantron.close()
  ret
}

/** Turns the unconnected 2D seq into a proper graph where neighboring vertices
 * can have the same color.
 *
 * @param pre_graph The 2D sequence to turn into a labelled graph
 * @return A labelled graph corresponding to the input grid of colors
 * @author Evan Dreher
 */
@unused def graphify_pre_graph(pre_graph: Seq[Seq[Int]]): Graph = {
  val (rows, cols) = (pre_graph.length, pre_graph.head.length)
  val flat = pre_graph.flatten

  // helper function to get orthogonally adjacent nodes of a vertex
  def get_edges(index: Int): List[(Int, Int)] = {
    val lastRow = index >= (rows * cols) - cols
    val lastCol = index % cols == (cols - 1)
    (lastCol, lastRow) match
      case (true, true) => List()
      case (true, false) => List((index, index + cols))
      case (false, true) => List((index, index + 1))
      case _ => List((index, index + cols), (index, index + 1))
  }

  val vertices = flat.indices
  val g: Graph = Graph(vertices, (vertices flatMap get_edges).toList)
  vertices.foreach(i => g.labels(i) = flat(i))
  g
}

/** Simplifies a grid graph by merging neighbors with identical labels into one
 * vertex while maintaining edges in the original graph.
 *
 * @param g A grid graph with randomly colored vertices
 * @return A graph that is color-wise identical to the input graph but with
 *         neighboring vertices of the same color clustered together as 1 Vertex
 * @author Evan Dreher
 */
@unused def blobify(g: Graph): Graph = {
  // a mutable map used to assign each vertex to a "blob" neighboring vertices with equal labels
  // will be assigned to the same blob which translates to a single vertex in the returned graph
  val blobs: MutMap[Int, Int] = MutMap()

  /** Performs depth first search on a vertex in the graph, only
   * vertices with equal labels to the input vertex will be searched
   *
   * @param v the vertex to start the depth first search from
   * @return An array of booleans indicating (by index) which vertices are in v's blob
   */
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

  // assign every vertex to its blob
  var next_blob: Int = 0
  for v <- g.vertices do if !(blobs contains v) then {
    val blob: Array[Boolean] = dfs(v) // find the blob
    for b <- blob.indices; if blob(b) do blobs.put(b, next_blob) // assign blob vertices to blob
    next_blob = next_blob + 1 // increment blob counter
  }

  // adjusts edges to maintain the graph connectivity
  val edges = (for edge <- g.edges; if blobs(edge._1) != blobs(edge._2) yield {
    val to_prime = blobs(edge._1)
    val from_prime = blobs(edge._2)
    if to_prime > from_prime then (from_prime, to_prime) else (from_prime, to_prime)
  }).distinct

  // assign labels to the blobbed vertices
  val g_prime: Graph = Graph(0 to blobs.values.max, edges)
  for v <- g.vertices do g_prime.labels(blobs(v)) = g.labels(v)
  g_prime
}

/** Sends a graph of your choice to the shadow realm (SSD) as a .txt file so it
 * can be read later.
 *
 * @param g    The graph to be sent away.
 * @param name The name of the file you want to save the graph to.
 * @author Evan Dreher
 */
@unused def send_graph_to_shadow_realm(g: Graph, name: String): Unit = {
  val f: File = new File(s"./samples/$name")
  val pw: PrintWriter = new PrintWriter(f)
  pw.write(s"${g.vertices.end + 1} ${g.edges.length}\n")
  pw.write(s"${g.edges.map((t, f) => s"$t $f").mkString("", "\n", "")}\n")
  pw.write(s"${g.labels.mkString("", " ", "")}")
  pw.flush()
}

/** Retrieves a graph from the shadow realm (SSD) by reading it from the
 * specified file path in ./samples
 *
 * @param name The name of the file at which the graph is stored
 * @return The graph stored at the file
 * @author Evan Dreher
 */
def retrieve_graph_from_shadow_realm(name: String): Graph = {
  val scantron: Scanner = new Scanner(new File(s"./samples/$name"))

  def readInt: Int = scantron.nextInt

  val (num_vertices, num_edges) = (readInt, readInt)
  val edges: List[(Int, Int)] = (1 to num_edges).map(_ => (readInt, readInt)).toList
  val labels: Array[Int] = (1 to num_vertices).map(_ => readInt).toArray
  val g: Graph = Graph(0 until num_vertices, edges)
  for i <- labels.indices do g.labels(i) = labels(i)
  g
}

/** Gets all experimental graphs of a certain class
 *
 * @param size   the size of the experiment
 * @param colors the number of colors in the experiment
 * @return a list of the 5 test cases for this class
 * @author Evan Dreher
 */
def get_experiments(size: Int, colors: Int): List[Graph] = {
  (for i <- 0 until 5 yield {
    retrieve_graph_from_shadow_realm(s"experiments\\${size}_by_$size\\${colors}_colors\\exp_$i.txt")
  }).toList
}

/** Gets a single experimental graphs of a certain class
 *
 * @param size   the size of the experiment
 * @param colors the number of colors in the experiment
 * @param id     the experiment id
 * @return the specified experiment
 * @author Evan Dreher
 */
def get_experiment(size: Int, colors: Int, id: Int): Graph = get_experiments(size, colors)(id)

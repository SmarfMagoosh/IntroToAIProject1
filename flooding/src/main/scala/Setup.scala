import scala.util.Random
import java.io.File
import java.util.Scanner
import java.io.PrintWriter

/**
 * Generates a random grid of colored nodes
 * @param rows the number of rows for the graph
 * @param cols the number of columns for the graph
 * @param colors the number of colors each vertex on the graph could be
 * @return
 */
def pre_graph(rows: Int, cols: Int, colors: Int): Seq[Seq[Int]] = {
  (0 until rows).map(_ => (0 until cols).map(_ => Random.nextInt(colors)))
}

/**
 * Reads a grid of nodes from a the specified /samples. The first number in the file is the number
 * of rows then the number of columns the rest of the file is the color_id for each respective node.
 * This method utilizes java.scanner.nextInt so any separation of the numbers will suffice.
 * @param name the path to the file with the specified graph
 * @return
 */
def pre_graph_from_file(name: String): Seq[Seq[Int]] = {
  val scantron: Scanner = new Scanner(new File(s"./samples/$name"))
  val (rows, cols) = (scantron.nextInt, scantron.nextInt)
  val ret = for row <- 0 until rows yield for col <- 0 until cols yield scantron.nextInt
  scantron.close()
  ret
}

/**
 * Turns the unconnected 2D seq into a proper graph
 * @param pre_graph The 2D sequence to turn into a labelled graph
 * @return A labelled graph corrsesponding to the input pre_graph
 */
def graphify_pre_graph(pre_graph: Seq[Seq[Int]]): Graph = {
  val (rows, cols) = (pre_graph.length, pre_graph.head.length)
  val flat = pre_graph.flatten
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

/**
 * sends a pregraph to the shadow realm (SSD) as a .txt file
 * @param name the name of the file
 * @param pre_graph the graph to be sent to the shadow realm
 */
def send_graph_to_shadow_realm(name: String, pre_graph: Seq[Seq[Int]]): Unit = {
  val f: File = File(s"./samples/$name.txt")
  f.createNewFile()
  val pw: PrintWriter = PrintWriter(f)
  pw.write(s"${pre_graph.length} ${pre_graph.head.length}\n")
  pw.write(pre_graph.map(_.mkString(" ")).mkString("\n"))
  pw.close()
}

@main def main(): Unit = {
  val pg = pre_graph(3, 4, 5)
  val g = graphify_pre_graph(pg)
  println(pg)
  println(g)
}

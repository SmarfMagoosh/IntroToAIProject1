import java.util.Scanner
import java.io.File
import java.io.PrintWriter
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

@main def main(): Unit = {
  val files = (for i <- 0 to 1; j <- 0 to 2 yield {
    new File(s"./samples/experiment_results/A_Star/A_Star_${i+1}_h${j+1}.csv")
  })
  val writers = files.map(new PrintWriter(_))
  writers.foreach(_.write("Size, Colors, ID, Nodes Explored, Runtime, Solution\n"))
  (5 to 25 by 5).foreach(size => {
    println(s"Size: $size")
    (4 to 8).foreach(colors => {
      println(s"  Colors: $colors")
      (0 to 4).foreach(id => {
        println(s"    Experiment #${id + 1}")
        (0 to 1).foreach(algorithm => {
          (0 to 2).foreach(heuristic => {
            val writer = writers((3 * algorithm) + heuristic)
            val fr: Future[Result] = Future(run_experiment(size, colors, id, heuristic, algorithm))
            try {
              Await.result(fr, 5.minutes)
              fr.onComplete(tr => {
                val result = tr.get
                writer.write(s"$size, $colors, $id, ${result.nodes_explored}, ${result.runtime}, ${result.solution.mkString("[", " ", "]")}\n")
                writer.flush()
              })
            } catch {
              case _ =>
                writer.write(s"$size, $colors, $id, DNF, --, --\n")
                writer.flush()
            }
          })
        })
      })
    })
  })
  writers.foreach(_.close())
}

def run_experiment(size: Int, colors: Int, id: Int, heuristic: Int, algorithm: Int): Result = {
  val g = get_experiment(size, colors, id)
  val h = heuristic match
    case 0 => h1
    case 1 => h2
    case 2 => h3
  val algo = algorithm match
    case 0 => A_star
    case 1 => A_star_2
  val s1 = System.currentTimeMillis()
  val soln1 = algo(g)(h)
  Result(soln1._1, System.currentTimeMillis() - s1, soln1._2)
}
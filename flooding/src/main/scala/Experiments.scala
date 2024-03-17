import java.util.Scanner
import java.io.File
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

/** A simple storage class to keep track of the results of an experiment
 *
 * @param nodes_explored the number of nodes explored during the experiment
 * @param runtime        the time it took to find a solution
 * @param solution       the list of actions found to get to a goal state
 * @author Evan Dreher
 */
case class Result(nodes_explored: Int, runtime: Long, solution: List[Int])

/** Performs all experiments using both implementations of A* with both algorithms
 * Estimated runtime of approximately 35 hours in total
 *
 * @param size   optionally restrict the size of the experiments
 * @param colors optionally restrict the number of colors for the experiments
 * @param algorithms optionall restrict which algorithms are tested
 * @param heuristics optionally restrict which heuristics are used
 * @param indices   optionally restrict which experiment indices to try
 * @author Evan Dreher
 */
def experiment_suite(
                      size: (Int, Int) = (5, 25),
                      colors: (Int, Int) = (4, 8),
                      algorithms: (Int, Int) = (0, 1),
                      heuristics: (Int, Int) = (0, 1),
                      indices: (Int, Int) = (0, 4),
                      verbose: Int = 0): Unit = {
  // locate files to store results and add writers for them
  val files = for i <- 0 to 1; j <- 0 to 1 yield {
    new File(s"./samples/Backups/A_Star_${i + 1}_h${j + 1}.csv")
  }
  val writers = files.map(new FileWriter(_))
  var ctr = 1
  for {
    // iterate over everything
    size <- size._1 to size._2 by 5
    colors <- colors._1 to colors._2
    id <- indices._1 to indices._2
    algorithm <- algorithms._1 to algorithms._2
    heuristic <- heuristics._1 to heuristics._2
  } do {
    println(s"Experiment #$ctr")
    ctr = ctr + 1
    val writer = writers((2 * algorithm) + heuristic)
    // asynchronously run experiment
    val fr: Future[Result] = Future {
      try run_experiment(size, colors, id, heuristic, algorithm, verbose)
      catch case _ => Result(-1, -1L, List.empty) // out of memory
    }
    try {
      // wait 5 minutes for result, if it completes, return the result, otherwise return None
      val result = Await.result(fr, 5.minutes)
      if result.nodes_explored > 0 then // successful experiment
        writer.write(s"$size, $colors, $id, ${result.nodes_explored}, ${result.runtime}, ${result.solution.mkString("[", " ", "]")}\n")
      else
        writer.write(s"$size, $colors, $id, OOM, --, --\n") // out of memory
    } catch case _ => writer.write(s"$size, $colors, $id, DNF, --, --\n") // out of time
    writer.flush()
  }
  writers.foreach(_.close())
}

/** Runs a single experiment limiting the runtime to 5 minutes
 *
 * @param size      the size of the experiment
 * @param colors    the number of colors involved in the experiment
 * @param id        the index
 * @param heuristic which heuristic for A* to use
 * @param algorithm which implementation of A* to use (memory efficient version or fast version)
 * @return The result of the experiment including nodes explored, runtime, and solution
 * @author Evan Dreher
 */
def run_experiment(size: Int, colors: Int, id: Int, heuristic: Int, algorithm: Int, verbose: Int): Result = {
  // load up the graph from storage
  val g = get_experiment(size, colors, id)
  val h = heuristic match
    case 0 => h1
    case 1 => h2
  val algo = algorithm match
    case 0 => A_star
    case 1 => A_star_2
  // time how long it takes to get a solution
  val s1 = System.currentTimeMillis()
  try {
    val solution = algo(g, verbose)(h)
    Result(solution._1, System.currentTimeMillis() - s1, solution._2)
  } catch {
    // throw out of memory error to be caught elsewhere
    case _: OutOfMemoryError => throw new OutOfMemoryError("Ran out of memory before solution was found")
  }
}

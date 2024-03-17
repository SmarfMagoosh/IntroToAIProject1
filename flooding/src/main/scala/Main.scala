@main def main(
    size: Int,
    colors: Int,
    id: Int,
    heuristic: Int,
    algorithm: Int,
    verbose: Int
): Unit = {
  val r = run_experiment(size, colors, id, heuristic, algorithm, verbose)
  println(s"\nSolution found: ${r.solution.mkString(" -> ")}\n")
  val g = get_experiment(size, colors, id)
  println(s"Graph before actions: \n$g\n")
  println(s"Proof of solution: ${r.solution.foldLeft(g)(_ pick _)}")
}


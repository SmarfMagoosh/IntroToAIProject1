@main def main(size: Int, colors: Int, id: Int, heuristic: Int, algorithm: Int, verbose: Int): Unit = {
  val r = run_experiment(size, colors, id, heuristic, algorithm, verbose)

  println(s"\nSolution found: ${r.solution.mkString(" -> ")}\n")
  val g = get_experiment(size, colors, id)

  println(s"Graph before actions:\n$g\n")

  println("Proof of solution:")
  println(s"${r.solution.foldLeft(g)((graph, action) => {
    val g_prime = graph pick action
    if verbose > 0 && !g_prime.isGoal then println(s"Action: $action\nGraph after action:\n$g_prime\n")
    g_prime
  })}")
}
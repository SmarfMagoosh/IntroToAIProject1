@main def main(): Unit = {
  println("Running Main Experiments")
  val r = run_experiment(10, 5, 0, 1, 0, 2)
  println(r.solution)
}
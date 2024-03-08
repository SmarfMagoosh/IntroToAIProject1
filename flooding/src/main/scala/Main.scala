@main def main(): Unit = {
  val g = retrieve_graph_from_shadow_realm("test.txt")
  val soln = A_star(g)(h1)
  val soln2 = A_star(g)(h2)
  println(s"Number of Nodes explored for solution 1: ${soln._1}")
  println(s"Solution 1: ${soln._2.mkString("[", ", ", "]")}")
  println(s"Number of steps in solution 1: ${soln._2.length}")
  println(s"Result of solution 1:\n${soln._2.foldLeft(g)(_ pick _)}\n")
  println(s"Number of Nodes explored for solution 2: ${soln2._1}")
  println(s"Solution 2: ${soln2._2.mkString("[", ", ", "]")}")
  println(s"Number of steps in solution 2: ${soln2._2.length}")
  println(s"Result of solution 2:\n${soln2._2.foldLeft(g)(_ pick _)}\n")
}

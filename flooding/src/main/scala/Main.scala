type Action = (Int, Int) // first int is color we pick, second is vertex we're recoloring


@main def main(): Unit = {
  val empty_graph = Graph(0 until 1, List())
  println(empty_graph.vertices.end)
}

import scala.collection.mutable.{Stack, ArrayBuffer as MutArray}
import scala.util.control.Breaks._

// (fmin, fmax, f-cost of last explored node, hashcode of last explored graph)
type BeamStackItem = (Int, Int, Option[Int], Option[Int])

case class BeamStackSearchState(graph: Graph, path: List[Int], h: Int) {
  def getSuccessors(heuristic: Graph => Int): List[BeamStackSearchState] = {
    val actions = graph.adjacency(0).map(graph.labels(_)).distinct
    actions.map(color => {
      val successor = graph.pick(color)
      BeamStackSearchState(successor, color :: path, heuristic(successor))
    })
  }

  def f: Int = path.length + h

  override def toString: String =
    s"Path: ${path.mkString("[", ", ", "]")}\nHeuristic:$h\nGraph:\n$graph\n"
}

def beamStackSearch(heuristic: Graph => Int, beamWidth: Int)(
    start: Graph
): (Long, List[Int]) = {
  val layerOrdering = Ordering.by { (s: BeamStackSearchState) => (s, s) }(
    Ordering.Tuple2(
      Ordering.by[BeamStackSearchState, Int](_.f),
      Ordering.by[BeamStackSearchState, Int](_.graph.hashCode)
    )
  )

  var nodesExplored: Long = 1
  var costUpperLimit = (heuristic(start) + 1) * 2
  var bestSolution: List[Int] = List()
  var l = 0

  val beamStack = Stack[BeamStackItem]((0, costUpperLimit, None, None))
  val beam = MutArray[MutArray[BeamStackSearchState]](
    MutArray(BeamStackSearchState(start, List(), heuristic(start))), // layer 0
    MutArray() // layer 1
  )

  while (beamStack.nonEmpty) {
    breakable {
      for ((node, i) <- beam(l).zipWithIndex) {
        // check if goal state
        if node.graph.isGoal then
          bestSolution = node.path.reverse
          costUpperLimit = l
          nodesExplored -= beam(l).length - i + 1
          break

        // generate successors on next level
        for successor <- node.getSuccessors(heuristic) do
          // check if the successor is unexplored in the layer
          val lastNotPrunedCost = beamStack.top._3
          val lastNotPrunedHashCode = beamStack.top._4
          val isUnexplored =
            if lastNotPrunedCost.nonEmpty && lastNotPrunedCost.get == successor.f
            then successor.graph.hashCode > lastNotPrunedHashCode.get
            else true

          if isUnexplored &&
            successor.f >= beamStack.top._1 && successor.f < beamStack.top._2 &&
            !beam(l).map(_.graph).contains(successor.graph) &&
            !beam(l + 1).map(_.graph).contains(successor.graph)
          then beam(l + 1).append(successor)

        // prune the next layer
        if beam(l + 1).length > beamWidth then
          // sort the layer before pruning
          // max size of beamWidth + numColors.length - 1
          beam(l + 1) = beam(l + 1).sorted(layerOrdering)

          val fBestPruned = beam(l + 1)(beamWidth).f
          beam(l + 1) = beam(l + 1).take(beamWidth)

          val currentTop = beamStack.pop()
          beamStack.push(
            (
              currentTop._1,
              fBestPruned,
              currentTop._3,
              currentTop._4
            )
          )
      }
    }

    // go to next level if there's stuff to explore
    // backtrack if not
    if beam(l + 1).nonEmpty then
      // update the number of nodes explored
      nodesExplored += beam(l + 1).length

      // ensure the next layer is sorted
      // max size of beamWidth
      beam(l + 1) = beam(l + 1).sorted(layerOrdering)

      // ensure we resume from where we left off the next time we backtrack to this layer
      val worstNotPruned = beam(l + 1).last
      val currentTop = beamStack.pop()
      beamStack.push(
        (
          currentTop._1,
          currentTop._2,
          Some(worstNotPruned.f),
          Some(worstNotPruned.graph.hashCode)
        )
      )

      // continue on to next layer
      beamStack.push((0, costUpperLimit, None, None))
      beam.append(MutArray())
      l += 1
    else
      // Remove any other layers from the beamStack that aren't viable to generate nodes in.
      while beamStack.nonEmpty && beamStack.top._2 >= costUpperLimit do
        beamStack.pop()
        beam.dropRightInPlace(1)
        l -= 1

      // get rid of the items that are currently in the beam (aka explored last time on this layer)
      beam.last.clear()

      // update the stack top if there's still stuff to explore
      if beamStack.nonEmpty then
        val currentTop = beamStack.pop()
        beamStack.push(
          (
            currentTop._2,
            costUpperLimit,
            currentTop._3,
            currentTop._4
          )
        )
  }

  (nodesExplored, bestSolution)
}

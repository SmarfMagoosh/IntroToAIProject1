import scala.collection.mutable.{
  Stack,
  PriorityQueue,
  TreeSet,
  ArrayBuffer as MutArray
}
import scala.util.control.Breaks._

// (fmin, fmax, heuristic of last explored node, hashcode of last explored node)
type BeamStackItem = (Int, Int, Option[Int], Option[Int])

// TODO: use the State class from A_Star.scala
def beamStackSearch(heuristic: Graph => Int, colors: Range, beamWidth: Int)(
    start: Graph
): (Option[Int], Long) = {
  // the g cost is the same across the entire layer, so we can just use the heuristic
  val layerOrdering = Ordering.by { (g: Graph) => (g, g) }(
    Ordering.Tuple2(
      Ordering.by[Graph, Int](heuristic(_)),
      Ordering.by[Graph, Int](_.hashCode())
    )
  )

  var nodesExplored: Long = 1
  var costUpperLimit = (heuristic(start) + 1) * 2
  var bestSolution: Option[Int] = None
  var l = 0

  val beamStack = Stack[BeamStackItem]((0, costUpperLimit, None, None))
  val beam = MutArray[MutArray[Graph]](
    MutArray(start), // layer 0
    MutArray() // layer 1
  )

  while (beamStack.nonEmpty) {
    breakable {
      for (node <- beam(l)) {
        // check if goal state
        if node.vertices.length == 1 then
          bestSolution = Some(l)
          costUpperLimit = l
          // println(s"Found solution at depth $l")
          break

        // generate successors on next level
        for c <- colors if c != node.labels.head do
          val successor = node.pick(c)
          val f = (l + 1) + heuristic(successor) // g + h

          // check if the successor is unexplored in the layer
          val lastNotPrunedCost = beamStack.top._3
          val lastNotPrunedHashCode = beamStack.top._4
          val isUnexplored =
            if lastNotPrunedCost.nonEmpty && lastNotPrunedCost.get == f then
              successor.hashCode() > lastNotPrunedHashCode.get
            else true

          if isUnexplored && !beam(l).contains(
              successor
            ) && !beam(l + 1).contains(
              successor
            ) && f >= beamStack.top._1 && f < beamStack.top._2
          then beam(l + 1).append(successor)

        // prune the next layer
        if beam(l + 1).length > beamWidth then
          // sort the layer before pruning
          // max size of beamWidth + colors.length - 1
          beam(l + 1) = beam(l + 1).sorted(layerOrdering)

          val fBestPruned =
            (l + 1) + heuristic(beam(l + 1)(beamWidth)) // g + h
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
          Some(l + 1 + heuristic(worstNotPruned)),
          Some(worstNotPruned.hashCode())
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

  (bestSolution, nodesExplored)
}

/*
 * Beam Stack Search
 * used pseudocode from https://cdn.aaai.org/ICAPS/2005/ICAPS05-010.pdf to implement parts of the algorithm,
 * although I used normal beam stack search instead of DCBSS that the paper has pseudocode for.
 * @author Micah Nicodemus
 * */

import scala.collection.mutable.{Stack, ArrayBuffer as MutArray}
import scala.util.control.Breaks.*
import scala.annotation.{tailrec, unused}
import scala.collection.mutable

// (f-value min, f-value max, f-cost of last explored node, hashcode of last explored graph)
type BeamStackItem = (Int, Int, Option[Int], Option[Int])

/** A state class for the Beam Stack algorithm
  *
  * @param graph
  *   the current graph
  * @param parent
  *   the parent graph of the current graph (allows for path reconstruction)
  * @param actionTaken
  *   the action that was taken from the parent to get to the current graph
  * @param f
  *   the f-cost to get to the current graph
  * @author
  *   Micah Nicodemus
  */
@unused case class BeamStackSearchState(
    graph: Graph,
    parent: Option[BeamStackSearchState],
    actionTaken: Byte, // max 127 colors possible. -1 represents no action taken (will only be like that for start node)
    f: Int
) {

  /** Generates the successors of the current state
    *
    * @param heuristic
    *   the heuristic function to use to calculate the h-cost of the successors
    * @param layer
    *   the current layer of the algorithm, used to calculate the f-cost of the
    *   successors
    * @return
    *   a list of the successors of the current state
    */
  def getSuccessors(
      heuristic: Graph => Int
  )(layer: Int): List[BeamStackSearchState] = {
    val actions = graph.adjacency(0).map(graph.labels(_)).distinct
    actions.map(color => {
      val successor = graph.pick(color)
      BeamStackSearchState(
        successor,
        Some(this),
        color.toByte,
        (layer + 1) + heuristic(successor)
      )
    })
  }

  /** Reconstructs the path from the start node to the current node (aka gets
    * the list of actions taken to solve the problem)
    *
    * @param currPath
    *   the current path in the process of being reconstructed
    * @return
    *   the path from the start node to the current node (aka the list of
    *   actions taken)
    */
  @tailrec
  final def path(currPath: List[Byte] = List()): List[Byte] = parent match {
    case Some(p) => p.path(actionTaken :: currPath)
    case None    => currPath
  }
}

/** The Beam Stack Search algorithm
  *
  * @param start
  *   the start graph
  * @param beamWidth
  *   the width of the beam
  * @param verbosity
  *   the verbosity level of the algorithm's output to the terminal (0 = no
  *   output, 1 = general progress output, 2 = detailed output)
  * @param heuristic
  *   the heuristic function to use to calculate the h-cost of the successors
  * @return
  *   a tuple containing the number of nodes explored and the best solution to
  *   the problem
  */
def beamStackSearch(
    start: Graph,
    beamWidth: Int,
    verbosity: Byte = 0
)(
  heuristic: Graph => Int
): (Long, List[Byte]) = {
  if verbosity > 0 then println("Setting up Beam Stack Search")

  val layerOrdering = Ordering.by { (s: BeamStackSearchState) => (s, s) }(
    Ordering.Tuple2(
      Ordering.by[BeamStackSearchState, Int](_.f),
      Ordering.by[BeamStackSearchState, Int](_.graph.hashCode)
    )
  )

  var nodesExplored: Long = 1
  var costUpperLimit = (heuristic(start) + 1) * 2
  var bestSolution: List[Byte] = List()
  var l = 0

  // initialize the first layer (the one with start node) on the beam stack
  // and the first two layers of the beam
  val beamStack = mutable.Stack[BeamStackItem]((0, costUpperLimit, None, None))
  val beam = MutArray[MutArray[BeamStackSearchState]](
    MutArray(
      BeamStackSearchState(start, None, -1, heuristic(start))
    ), // layer 0 (with start node)
    MutArray() // layer 1
  )

  if verbosity > 0 then println("Starting Beam Stack Search")

  while (beamStack.nonEmpty) {
    if verbosity > 0 then
      println(
        s"Layer $l: Generating successors for layer ${l + 1} in the range [${beamStack.top._1}, ${beamStack.top._2}) from ${beam(l).length} nodes: ${beam(
            l
          ).map(_.graph.hashCode).mkString(", ")}"
      )

    breakable {
      // explore the nodes in the current layer
      for ((node, i) <- beam(l).zipWithIndex) {
        // check if goal state
        if node.graph.isGoal then
          bestSolution = node.path()
          costUpperLimit = l
          nodesExplored -= beam(l).length - i + 1
          if verbosity > 0 then println(s"Goal found at level: $l")
          if verbosity > 1 then
            println(s"Solution Found: ${bestSolution.mkString(", ")}")
            println(s"Nodes Explored so far: $nodesExplored")
          break

        // generate successors on next level
        for successor <- node.getSuccessors(heuristic)(l) do
          // Check if the successor is unexplored in the layer.
          // Solves the issue of repeatedly exploring the same nodes
          // caused by having non-unique f-costs in a layer.
          val lastNotPrunedCost = beamStack.top._3
          val lastNotPrunedHashCode = beamStack.top._4
          val isUnexplored =
            if lastNotPrunedCost.nonEmpty && lastNotPrunedCost.get == successor.f
            then successor.graph.hashCode > lastNotPrunedHashCode.get
            else true

          // add the successor to the next layer if it's currently a valid candidate
          if isUnexplored &&
            successor.f >= beamStack.top._1 && successor.f < beamStack.top._2 &&
            !beam(l).map(_.graph).contains(successor.graph) &&
            !beam(l + 1).map(_.graph).contains(successor.graph)
          then
            beam(l + 1).append(successor)
            if verbosity > 1 then
              println(s"Added successor: ${successor.hashCode}")

        // prune the next layer
        if beam(l + 1).length > beamWidth then
          if verbosity > 0 then
            println(
              s"Pruning layer ${l + 1} from ${beam(l + 1).length} nodes to $beamWidth nodes."
            )

          // sort the layer before pruning
          // max size of beamWidth + numColors.length - 1
          beam(l + 1) = beam(l + 1).sorted(layerOrdering)

          if verbosity > 1 then
            println(
              s"Pruned nodes: ${beam(l + 1).drop(beamWidth).map(_.graph.hashCode).mkString(", ")}"
            )

          // chop chop
          val fBestPruned = beam(l + 1)(beamWidth).f
          beam(l + 1) = beam(l + 1).take(beamWidth)

          // update the range we generated nodes in
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

    if verbosity > 0 then
      println(s"Generated ${beam(l + 1).length} nodes for layer ${l + 1}")

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
      // Remove any layers from the top of the beamStack that aren't viable to generate nodes in.
      while beamStack.nonEmpty && beamStack.top._2 >= costUpperLimit do
        beamStack.pop()
        beam.dropRightInPlace(1)
        l -= 1

      // get rid of the items that are currently in the beam (aka explored last time on this layer)
      beam.last.clear()

      // shift the range of the beam stack top so we explore new nodes next time we backtrack to this layer
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

      if verbosity > 0 then
        println(
          s"Backtracking to layer $l"
        )
  }

  if verbosity > 0 then
    println("Beam Stack Search Complete")
    println(s"Nodes Explored: $nodesExplored")
    println(s"Best Solution: ${bestSolution.mkString(", ")}")

  (nodesExplored, bestSolution)
}

package net.thenobody.codinginterviews.hackerrank.graphtheory

/**
 * Created by antonvanco on 17/02/2016.
 */
object GraphTheory {

  def readInt = io.StdIn.readLine().filter(_ != ' ').mkString.toInt
  def readPair: (Int, Int) = io.StdIn.readLine().split(' ') match { case Array(x, y) => (x.toInt, y.toInt); case x => throw new IllegalArgumentException(x.mkString("#")) }

  def breadthFirstSearchShortestReach(): Unit = {
    def go(currentNodes: Seq[Int], nodes: Seq[Int], distance: Int, edges: Map[Int, Seq[Int]]): Seq[(Int, Int)] = {
      val availableNext = currentNodes.flatMap(edges.getOrElse(_, Seq())).toSet
      val (nextNodes, remainingNodes) = nodes.foldLeft((Seq.empty[Int], Seq.empty[Int])) { case ((next, remaining), node) =>
        if (availableNext.contains(node)) (next :+ node, remaining)
        else (next, remaining :+ node)
      }
//      val nextNodes = currentNodes.flatMap(edges).distinct.filter(nodes.contains)
      if (nextNodes.isEmpty) Seq.empty
      else {
        val distances = nextNodes.map(_ -> distance)
//        val remainingNodes = nodes.filterNot(nextNodes.contains)
        val allDistances = distances ++ go(nextNodes, remainingNodes, distance + 1, edges)
        allDistances.groupBy(_._1).mapValues(_.map(_._2).min).toSeq
      }
    }

    val count = readInt
    (0 until count).foreach { _ =>
      val (nodeCount, edgeCount) = readPair
      val nodes = 1 to nodeCount
      val edges = (0 until edgeCount).flatMap { _ =>
        val (x, y) = readPair
        Seq((x, y), (y, x))
      }.groupBy(_._1).mapValues(_.map(_._2))
      val startNode = readInt

      val searchNodes = nodes.filter(_ != startNode)

      val distances = go(Seq(startNode), searchNodes, 1, edges).toMap
      val result = searchNodes.sorted.map(distances.get(_).map(_ * 6)).map {
        case Some(distance) => distance
        case None => -1
      }
      println(result.mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    breadthFirstSearchShortestReach()
  }

}

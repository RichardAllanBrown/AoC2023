package com.rab.aoc

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

object Day25 {
  case class Graph(nodes: Seq[String], edges: Seq[Seq[Int]]) {
    private lazy val nodeToIndex = nodes.zipWithIndex.toMap

    def addNode(n: String): Graph = {
      if nodes.contains(n) then this
      else Graph(nodes :+ n, edges :+ Seq.empty)
    }

    def addEdge(u: String, v: String): Graph = {
      val ui = nodeToIndex(u)
      val vi = nodeToIndex(v)
      val newEdges = edges.updated(ui, edges(ui) :+ vi)
        .updated(vi, edges(vi) :+ ui)
      copy(edges = newEdges)
    }

    def removeEdge(u: String, v: String): Graph = {
      val ui = nodeToIndex(u)
      val vi = nodeToIndex(v)
      val newEdges = edges.updated(ui, edges(ui).filterNot(_ == vi))
        .updated(vi, edges(vi).filterNot(_ == ui))
      copy(edges = newEdges)
    }

    def apply(u: String): Seq[String] = {
      edges(nodeToIndex(u)).map(nodes(_))
    }

    def findPath(start: String, target: String): Seq[String] = {
      @tailrec
      def bfs(toExplore: Queue[String], prev: Map[String, String]): Map[String, String] = {
        toExplore.headOption match
          case None => prev
          case Some(n) =>
            val newNeighbours = apply(n).filterNot(prev.contains)
            val newToExplore = toExplore.tail.appendedAll(newNeighbours)
            bfs(newToExplore, prev ++ newNeighbours.map(_ -> n))
      }
      val prevPaths = bfs(Queue(start), Map.empty)

      @tailrec
      def getPathToStart(p: List[String]): Seq[String] = {
        if p.head == start then p else getPathToStart(prevPaths(p.head) :: p)
      }
      getPathToStart(List(target)).reverse
    }

    def countReachableNodes(from: String): Int = {
      @tailrec
      def bfs(toExplore: Queue[String], visited: Set[String]): Set[String] = {
        toExplore.headOption match
          case None => visited
          case Some(n) =>
            val newNeighbours = apply(n).filterNot(visited.contains)
            val newToExplore = toExplore.tail.appendedAll(newNeighbours)
            bfs(newToExplore, visited ++ newNeighbours)
      }
      bfs(Queue(from), Set.empty).size
    }
  }

  object Graph {
    val empty: Graph = Graph(Seq.empty, Seq.empty)
    def ofNodes(n: Set[String]): Graph = Graph(n.toSeq, Seq.empty)
  }

  def parse(lines: List[String]): Graph = {
    val entries = lines.map(l => {
      l.split(Array(':', ' ')).map(_.trim).filter(_.nonEmpty) match
        case Array(h, t*) => h -> t
    })

    val gWithNodes = entries.flatMap(a => a._2 :+ a._1).toSet
      .foldLeft(Graph.empty)(_.addNode(_))

    entries.foldLeft(gWithNodes)((g, e) => {
      val (source, destinations) = e
      destinations.foldLeft(g)(_.addEdge(source, _))
    })
  }

  def findRandomPath(g: Graph): Seq[String] = {
    Random.shuffle(g.nodes).take(2) match
      case Seq(a, b) => g.findPath(a, b)
  }

  def findPopularEdges(g: Graph, trials: Int = 2000): Seq[((String, String), Int)] = {
    LazyList.from(1).take(trials)
      .map(i => {
        if i % 100 == 0 then println(s"Running trial $i") else ()
        findRandomPath(g)
      })
      .flatMap(_.sliding(2).map(_.sorted).map { case Seq(a, b) => (a, b) })
      .groupBy(identity)
      .view.mapValues(_.length)
      .toSeq
      .sortBy(_._2)
      .reverse
  }

  def solvePart1(lines: List[String]): Int = {
    val graph = parse(lines)
    println(s"Graph has ${graph.nodes.length} nodes and ${graph.edges.map(_.length).sum/2} edges")
    val edgeCount = findPopularEdges(graph)
    val mostPopularEdges = edgeCount.map(_._1).take(3)
    println("The edges to remove are:")
    mostPopularEdges.foreach(println)
    val graphWithoutEdges = mostPopularEdges.foldLeft(graph)((g, e) => g.removeEdge(e._1, e._2))
    mostPopularEdges.head match
      case (a, b) =>
        graphWithoutEdges.countReachableNodes(a) *
        graphWithoutEdges.countReachableNodes(b)
  }
}

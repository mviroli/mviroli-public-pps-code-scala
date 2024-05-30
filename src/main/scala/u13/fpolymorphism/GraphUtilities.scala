package u13.fpolymorphism

// Some utilities to work for any Graph family
object GraphUtilities:
  // g.Edge is a path-dependent type.. the Edge-type of g
  def getEdges(g: Graph): Set[g.Edge] =
    g.nodes flatMap (g outEdges _)
  // currying is here needed by Scala, to correctly type g.Edge as argument
  def getSource(g: Graph)(e: g.Edge): Option[g.Node] =
    g.nodes find (g outEdges _ contains e)
  def getTarget(g: Graph)(e: g.Edge): Option[g.Node] =
    g.nodes find (g inEdges _ contains e)

object UseGraphUtilities extends App:

  // Note how the trait ColouredWeightedGraph mixes-in the abstract implementation
  val g = new GraphImpl() with MyNodesAndEdges with ColouredWeightedGraph

  g.addEdge(g.Node("a",1),g.Node("b",2),g.Edge(1.0))
  g.addEdge(g.Node("c",3),g.Node("d",4),g.Edge(2.0))
  g.addEdge(g.Node("a",1),g.Node("d",4),g.Edge(3.0))

  // Scala correctly types g.Edge as being MyEdge
  val nodes: Set[g.Edge] = GraphUtilities.getEdges(g)
  println(nodes) // Set(Edge(1.0), Edge(3.0), Edge(2.0))
  println(GraphUtilities.getSource(g)(g.Edge(1.0))) // Some(Node(a,1))
  println(GraphUtilities.getSource(g)(g.Edge(1.0)).get.colour) // 1
  println(GraphUtilities.getTarget(g)(g.Edge(1.0))) // Some(Node(b,2))

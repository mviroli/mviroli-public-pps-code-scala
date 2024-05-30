  package u13.fpolymorphism

trait ColoredNode:
  def colour: Int

trait WeightedEdge:
  def weight: Double

// Note the whole family of Node and Edge is here specialised by family polimorphism
trait ColouredWeightedGraph extends Graph:
  // Further-bounding Node and Edge with structural types
  type Node <: ColoredNode     // Nodes should have at least a colour
  type Edge <: WeightedEdge    // Edge should have at least a weight
  // a function using both colours and weights: summing weights out of nodes with colour
  def totalOutWeight(colour: Int): Double =
    nodes.filter(_.colour == colour).toList.flatMap(outEdges(_)).map(_.weight).sum

// A mixin specifying concrete nodes and edges for graphs
trait MyNodesAndEdges:
  self: Graph =>
  override case class Node(name: String, colour: Int) extends ColoredNode
  override case class Edge(weight: Double) extends WeightedEdge

object TryCWGraphs extends App:
  // Note how the trait ColouredWeightedGraph mixes-in the abstract implementation
  val g = new GraphImpl() with ColouredWeightedGraph with MyNodesAndEdges
  // Without a notion of family, we couldn't guarantee that Node and Edge belong to the same family
  g.addEdge(g.Node("a",1),g.Node("b",1),g.Edge(1.0))
  g.addEdge(g.Node("c",3),g.Node("d",4),g.Edge(2.0))
  g.addEdge(g.Node("a",1),g.Node("d",4),g.Edge(3.0))
  g.addEdge(g.Node("b",1),g.Node("d",4),g.Edge(1.0))
  println(g.totalOutWeight(1)) // 5.0



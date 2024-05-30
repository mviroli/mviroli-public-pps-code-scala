package u13.fpolymorphism

trait Graph:
  type Node   // Node and Edge are abstract types, member of Graph
  type Edge
  def addEdge(n1: Node, n2: Node, e: Edge): Unit
  def nodes: Set[Node]
  def outEdges(n: Node): Set[Edge]
  def inEdges(n: Node): Set[Edge]

// an implementation, still leaving Node and Edge as abstract
trait GraphImpl() extends Graph:
  private var data = Set[(Node,Edge,Node)]()
  override def addEdge(n1: Node, n2: Node, e: Edge) = data += ((n1,e,n2))
  override def nodes = (data map (_._1)) ++ (data map (_._3))
  override def outEdges(n: Node) = data collect { case (`n`,e,_) => e }
  override def inEdges(n: Node) = data collect { case (_,e,`n`) => e }

object TryGraph extends App:
  val g = new GraphImpl:  // an on-spot fully-concrete implementation
    override type Node = String
    override type Edge = Int

  g.addEdge("a","b",1)
  g.addEdge("c","d",2)
  g.addEdge("a","d",3)
  println((g.nodes,g.outEdges("a"),g.inEdges("d")))
  //(Set(a, c, b, d),Set(1, 3),Set(2, 3))
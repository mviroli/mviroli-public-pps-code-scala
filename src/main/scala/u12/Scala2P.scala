package u12

// with dependency: "it.unibo.alice.tuprolog" % "tuprolog" % "3.3.0"
import alice.tuprolog.*

object Scala2P:
  given Conversion[String, Term] = Term.createTerm(_)
  given Conversion[Seq[_], Term] = _.mkString("[",",","]")

  def extractTerm(t:Term, i:Integer): Term =
    t.asInstanceOf[Struct].getArg(i).getTerm

  def mkPrologEngine(clauses: String*): Term => LazyList[Term] =
    val engine = Prolog()
    engine.setTheory(Theory(clauses mkString " "))
    goal => new Iterable[Term]{
      override def iterator = new Iterator[Term] {
        var solution = engine.solve(goal);
        override def hasNext =
          solution.isSuccess || solution.hasOpenAlternatives
        override def next() =
          try { solution.getSolution} finally {solution = engine.solveNext }
      }
    }.to(LazyList)

object TryScala2P extends App:
  import Scala2P.{*, given}

  val engine: Term => LazyList[Term] = mkPrologEngine("""
    member([H|T],H,T).
    member([H|T],E,[H|T2]):- member(T,E,T2).
    permutation([],[]).
    permutation(L,[H|TP]) :- member(L,H,T), permutation(T,TP).
  """)
  /*
  engine("permutation([1,2,3],L)") foreach (println(_))
  // permutation([1,2,3],[1,2,3]) ... permutation([1,2,3],[3,2,1])
  */

  val input = Struct("permutation",(1 to 20), Var())
  engine(input) map (extractTerm(_,1)) foreach (println(_))
  // [1,2,3,4,..,20] ... [1,2,..,15,20,16,18,19,17]

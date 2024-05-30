package u13.pimp

trait PrologTerms:
  type Var
  type Atom
  enum Term:
    case V(v: Var)
    case Constant(c: AnyVal)
    case Compound(a: Atom, args: Term*)
  case class Clause(h: Term, b: Term*)

trait PrologSyntax:
  this: PrologTerms =>  //self-type
  extension (a: Atom) def apply(args: Term*) = Term.Compound(a, args:_*)
  given Conversion[AnyVal, Term.Constant] = Term.Constant(_)
  given Conversion[Var, Term.V] = Term.V(_)
  given Conversion[Term, Clause] = Clause(_)
  extension (t: Term) def :-(b: Term*) = Clause(t, b:_*)

object TrySyntax extends App with PrologTerms with PrologSyntax:
  enum MyVar:
    case H,T,H2
  enum MyAtom:
    case member,cons
  type Var = MyVar
  type Atom = MyAtom
  import MyVar.*, MyAtom.*

  val program: List[Clause] = List(
    member(H, cons(H,T)),
    member(H, cons(H2,T)) :- member(H, T)
  )
  println(program)

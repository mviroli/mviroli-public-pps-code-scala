package u13.parsing

import u13.parsing.Parsers.*

import u13.monad.Monad
import u13.monad.MonadOps.*
import Parsers.*
import ParsersDSL.{*,given}

object Expressions:

  enum Exp:
    case Num(n:Int)
    case Sum(l: List[Exp])
    case Mul(l: List[Exp])
  import Exp.*

  def expr: Parser[Exp] =
    (term ~ many("+" ~> term)) ^^ {case (t,Nil) => t; case (t,l) => Sum(t::l)}

  def term: Parser[Exp] =
    (factor ~ many("*" ~> factor)) ^^ {case (t,Nil) => t; case (t,l) => Mul(t::l)}

  def factor: Parser[Exp] =
    (natural ^^ {Num(_).asInstanceOf[Exp]}) // ||  "(" ~> expr <~ ")"

  def prettyPrint(e: Exp): String = e match {
    case Num(n) => n.toString
    case Sum(l) => l.map(prettyPrint(_)).mkString("(","+",")")
    case Mul(l) => l.map(prettyPrint(_)).mkString("(","*",")")
  }

  def compute(e: Exp): Int = e match
    case Num(n) => n
    case Sum(l) => l.foldRight(0)(compute(_)+_)
    case Mul(l) => l.foldRight(1)(compute(_)*_)


object TryExpressions extends App:
  import Expressions.*

  val res = parse(expr,"20+10*30").get
  println( res) // Sum(List(Num(20), Mul(List(Num(10), Num(30)))))
  println( prettyPrint(res)) // (20+(10*30))
  println( compute(res)) // 320

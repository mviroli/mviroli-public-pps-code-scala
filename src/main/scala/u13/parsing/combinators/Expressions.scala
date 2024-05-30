package u13.parsing.combinators

import scala.util.parsing.combinator._

// libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends Arith {
  def main(args: Array[String]): Unit = {
    val expString = "2 * (3 + 7))"
    println(parseAll(expr, expString))
  }
}
package u13.parsing

import u13.monad.Monad
import u13.monad.MonadOps.*
import Parsers.*

object ParsersDSL:

  given Conversion[String, Parser[String]] = str(_)

  def spaces = many(sym(' '))

  extension [A](p: Parser[A])
    def ^^[B](f: A => B): Parser[B] = p.map(f)
    def ~[B](p2: => Parser[B]): Parser[(A, B)] = seq(p <~ spaces, p2)
    def ~>[B](p2: => Parser[B]): Parser[B] = seq(p, p2) ^^ (_._2)
    def <~[B](p2: => Parser[B]): Parser[A] = seq(p, p2) ^^ (_._1)
    def |[B](p2: Parser[B]): Parser[Any] = or(p, p2)
    def ||(p2: Parser[A]): Parser[A] = or(p, p2)

object TryDSL extends App:

  import ParsersDSL.{*,given}

  def identifier = many1(letter)
  def par = identifier ~ ":" ~ identifier
  def arguments = "(" ~ ")" | "(" ~ par ~ many("," ~ par) ~ ")"
  def methodSignature =
    "def" ~ identifier ~ optional(null, arguments) ~ ":" ~ identifier

  println(parse(methodSignature, "def x: Int"))
  println(parse(methodSignature, "def x(): Int"))
  println(parse(methodSignature, "def x(n: Int, m: Double): Int"))

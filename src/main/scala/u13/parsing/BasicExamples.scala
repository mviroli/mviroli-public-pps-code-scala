package u13.parsing

import u13.monad.Monad
import u13.monad.MonadOps.*

object BasicExamples extends App :

  import Parsers.{*, given}

  println(item.parse("ciao")) // List((c,iao))
  println(sym('c').parse("ciao")) // List((c,iao))
  println(sym('b').parse("ciao")) // List()

  val tupleParser: Parser[(Char, Char)] =
    for a <- item; b <- item yield (a, b)
  println(tupleParser.parse("ciao")) //List(((c,guess),ao))

  val tripleStringParser: Parser[String] =
    for a <- item; b <- item; c <- item yield "" + a + b + c
  println(tripleStringParser.parse("ciao")) //List((cia,o))

  val parsingZeroOrOne: Parser[Char] =
    Parser[Char](s => sym('0').parse(s) ++ sym('1').parse(s))
  println(parsingZeroOrOne.parse("0ciao")) //List((0,ciao))
  println(parsingZeroOrOne.parse("1ciao")) //List((1,ciao))
  println(parsingZeroOrOne.parse("2ciao"))

package u13.parsing

import u13.monad.Monad
import u13.monad.MonadOps.*

object AdvancedExamples extends App :

  import Parsers.{*, given}

  println(seq(item, item).parse("ciao"))
  println(seqN(item, item, item).parse("ciao"))
  println(or(sym('a'), sym('b')).parse("abcd"))
  println(or(sym('a'), sym('b')).parse("bcd"))
  println(or(sym('a'), sym('b')).parse("cd"))
  println(optional(' ', sym('a')).parse("abc"))
  println(many1(sym('a')).parse("aaab"))
  println(many(sym('a')).parse("aaab"))
  println(str("ci").parse("ciao"))
  println(natural.parse("156aa"))

  def naturalList: Parser[List[Int]] = for
    _ <- sym('[')
    n <- natural
    ns <- many(for _ <- sym(','); x <- natural yield x)
    _ <- sym(']')
  yield n :: ns

  println(naturalList.parse("[100,200,131]"))

  println(parse(naturalList, "[100,200,300]"))

  def spaces = many(sym(' '))

  def seqNS[A](ps: Parser[A]*) =
    sequence(spaces :: ps.toList.flatMap(x => List(x, spaces)))

  def identifier = many1(letter)

  def argument = seqNS(identifier, sym(':'), identifier)

  def methodSignature = seqNS(
    str("def"), identifier, sym('('),
    optional(null,
      seqN(argument, many(seqN(sym(','), argument)))),
    sym(')')
  )

  println(parse(identifier, "abc"))
  println(parse(argument, "abc:de"))
  println(parse(argument, "  abc : de"))
  println(parse(methodSignature, "def x()"))
  println(parse(methodSignature, "def x(n: Int)"))
  println(parse(methodSignature, "def x(n: Int, d: Double)"))

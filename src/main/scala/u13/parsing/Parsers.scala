package u13.parsing

import u13.monad.Monad
import u13.monad.MonadOps.*

object Parsers:

  case class Parser[+A](parse: String => List[(A, String)])

  given Monad[Parser] with
    def unit[A](a: => A): Parser[A] = Parser(s => List((a, s)))

    extension[A] (p: Parser[A])
      def flatMap[B](f: A => Parser[B]): Parser[B] =
        Parser(s => p.parse(s) flatMap { case (a, rest) => f(a).parse(rest) })

  // successfully get symbols a, namely, unit
  def success[A](a: A): Parser[A] = summon[Monad[Parser]].unit(a)

  // fails parsing
  def failure[A]: Parser[A] = Parser(s => List())

  // a parser of any single char, failing on empty lists
  def item: Parser[Char] =
    Parser(s => if s.isEmpty then Nil else List((s.head, s.tail)))

  // gets an element if it satisfies a given predicate
  def conditional[A](p: Parser[A])(predicate: A => Boolean): Parser[A] =
    p.flatMap(a => if predicate(a) then success(a) else failure)

  // gets symbol c
  def sym(c: Char) = conditional(item)(_ == c)

  // a good entry-point, parsing to completion
  def parse[A](p: Parser[A], s: String): Option[A] =
    p.parse(s).collectFirst { case (a, "") => a }

  // gets a digit
  def digit = conditional(item)(_.isDigit)

  // gets a letter
  def letter = conditional(item)(_.isLetter)

  // gets a parser of an A and then a B into a parser of tuples
  def seq[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    map2(p1, p2)((_, _))

  // gets a sequence of elements of type A into a list
  def seqN[A](ps: Parser[A]*): Parser[List[A]] =
    sequence(ps.toList)

  // gets a parser that parses like p1 or like p2
  def or[A, B <: A, C <: A](p1: Parser[B], p2: Parser[C]): Parser[A] =
    Parser(s => p1.parse(s) ++ p2.parse(s))

  // gets 0 or 1 occurrence of A
  def optional[A](default: A = null, p: Parser[A]): Parser[A] =
    or(success[A](default), p)

  // gets 1, or many occurrences of A, resulting in a list
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // gets 0,1, or many occurrences of A, resulting in a list
  def many[A](p: Parser[A]): Parser[List[A]] =
    optional(List[A](), many1(p))

  // gets string s
  def str(s: String): Parser[String] =
    if (s.isEmpty) success("")
    else sequence(s.toList.map(sym(_))).map(_.mkString)

  // gets a natural
  def natural: Parser[Int] =
    for digits <- many1(digit)
      yield digits.map(_ - '0').foldLeft(0) { case (n, i) => 10 * n + i }
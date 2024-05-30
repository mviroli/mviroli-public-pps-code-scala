package u13.parsing

object Try extends App:

  case class Parser[T](parse: String => List[(T, String)]):
    // How this parser is used via f to produce the result?
    def flatMap[R](f: T => Parser[R]): Parser[R] = ???
    def map[R](f: T => R): Parser[R] = ???

  // not sure how to implement flatMap or these combinators, yet..
  def genericSymbol(): Parser[Char] = ???

  val parser = genericSymbol()
  println(parser.parse("bcd")) // List( ('b',"cd") )

  def twoGenericSymbols(): Parser[(Char,Char)] =
    for car1 <- genericSymbol(); car2 <- genericSymbol() yield (car1,car2)

  val parser2 = twoGenericSymbols()
  println(parser2.parse("abc")) // List( (('a','b'),"c") )

  def twoGenericSymbolsIntoString(): Parser[String] =
    for car1 <- genericSymbol(); car2 <- genericSymbol() yield ""+car1+car2

  val parser3 = twoGenericSymbolsIntoString()
  println(parser3.parse("abc")) // List( ("ab","c") )
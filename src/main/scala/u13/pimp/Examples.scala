package u13.pimp

object PimpingByWrapping:
  case class MyIntOp(value: Int):
    def ***(n: Int): Int = n match
      case 0 => 1
      case n => (value *** (n-1)) * value
    def square: Int = value *** 2

  given Conversion[Int, MyIntOp] = MyIntOp(_)

object PimpingByExtension:
  extension (value :Int)
    def ***(n: Int): Int = n match
      case 0 => 1
      case n => (value *** (n-1)) * value
    def square: Int = value *** 2

@main def pimpingByWrapping =
  import PimpingByWrapping.given
  println(5 *** 2)
  import language.postfixOps
  println(5 square)

@main def pimpingByExtension =
  import PimpingByExtension.*
  println(5 *** 2)
  import language.postfixOps
  println(5 square)
package u12

object LogicLookup extends App:

  // lookup([H|T],H,zero,T).
  // lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).

  def lookup[A](list : List[A]): LazyList[(A,Int,List[A])] = list match
    case Nil => LazyList()
    case h :: t =>
      (h,0,t) #::
      (for (e,n,t2) <- lookup(t) yield (e,n+1,h::t2))

  println( lookup( List(10,20,30,20) ).toList)
  // List((10,zero,List(20, 30, 20)), (20,s(zero),List(10, 30, 20)), ...

  println( lookup( List(10,20,30,20) ).collect{case (20,n,_) => n}.toList)
  // List(s(zero), s(s(s(zero))))

  println( lookup( List(10,20,30,40) ).collect{case (e,3,_) => e}.toList)
  // List(40)

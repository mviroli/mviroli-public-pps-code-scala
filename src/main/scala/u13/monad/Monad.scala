package u13.monad

trait Monad[F[_]]:
  def unit[A](a: => A): F[A]

  extension [A](ma: F[A])
    def flatMap[B](f: A => F[B]): F[B]

object MonadOps:
  extension [F[_]:Monad, A](ma: F[A])
    def map[B](f: A => B): F[B] =
      ma.flatMap(a => summon[Monad[F]].unit(f(a)))

  def map2[F[_]:Monad,A,B,C](ma: F[A], mb: =>F[B])(f: (A, B) => C): F[C] =
      ma.flatMap(a => mb.map(b => f(a, b)))

  def sequence[F[_]:Monad,A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(summon[Monad[F]].unit(List[A]()))(
      map2(_,_)(_ :: _))

  def traverse[F[_]:Monad,A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(summon[Monad[F]].unit(List[B]()))(
      (a, mlb) => map2(f(a),mlb)(_ :: _))

  def compose[F[_]:Monad,A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    f(_).flatMap(g)

  def join[F[_]:Monad,A](mma: F[F[A]]): F[A] =
    mma.flatMap(identity)
package fpinscala.iomonad

object api {
  import fpinscala.iomonad.io.IO3
  import fpinscala.parallelism.Nonblocking.*

  type Free[F[_], A] = IO3.Free[F, A]
  type IO[A] = IO3.IO[A]
  def IO[A](a: => A): IO[A] = IO3.IO[A](a)

  given ioMonad: Monad[[X] =>> Free[Par, X]] = IO3.freeMonad[Par]

  def now[A](a: A): IO[A] = IO3.Free.Return(a)

  def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())) flatMap (_ => a)

  def forkUnit[A](a: => A): IO[A] = fork(now(a))

  def delay[A](a: => A): IO[A] = now(()) flatMap (_ => now(a))

  def par[A](a: Par[A]): IO[A] = IO3.Free.Suspend(a)

  def async[A](cb: (A => Unit) => Unit): IO[A] =
    fork(par(Par.async(cb)))

  def Return[A](a: A): IO[A] = IO3.Free.Return[Par, A](a)

  // To run an `IO`, we need an executor service.
  // The name we have chosen for this method, `unsafePerformIO`,
  // reflects that is is unsafe, i.e. that it has side effects,
  // and that it _performs_ the actual I/O.
  import java.util.concurrent.ExecutorService
  def unsafePerformIO[A](io: IO[A])(using E: ExecutorService): A =
    Par.run(E) { IO3.run(io)(using IO3.parMonad) }
}

package fpinscala.iomonad.io

import fpinscala.iomonad.Monad

import scala.io.StdIn.readLine
import scala.language.implicitConversions

object IO3 {

  /** We can generalize `TailRec` and `Async` to the type `Free`, which is a `Monad` for any choice
   *  of `F`.
   */

  enum Free[F[_], A] {
    case Return(a: A)
    case Suspend(s: F[A])
    case FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    infix def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)

    infix def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  import Free.*

  // Exercise 1: Implement the free monad
  def freeMonad[F[_]]: Monad[[A] =>> Free[F, A]] =
    new Monad[[A] =>> Free[F, A]] {
      def unit[A](a: => A) = Return(a)
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]) = fa flatMap f
    }

  // Exercise 2: Implement a specialized `Function0` interpreter.
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = (a) match {
    case Return(a)     => a
    case Suspend(r)    => r()
    case FlatMap(x, f) => x match {
      case Return(a)      => runTrampoline { f(a) }
      case Suspend(r)     => runTrampoline { f(r()) }
      case FlatMap(a0, g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

  // Exercise 3: Implement a `Free` interpreter which works for any `Monad`
  def run[F[_], A](a: Free[F, A])(using F: Monad[F]): F[A] = step(a) match {
    case Return(a)              => F.unit(a)
    case Suspend(r)             => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _                      => sys.error("Impossible, since `step` eliminates these cases")
  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => a
  }

  /** The type constructor `F` lets us control the set of external requests our program is allowed
   *  to make. For instance, here is a type that allows for only console I/O effects.
   */

  import fpinscala.parallelism.Nonblocking.Par

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A

    // other interpreters
    def toState: ConsoleState[A]
    def toReader: ConsoleReader[A]
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case _: Exception => None }

    def toState = ConsoleState { bufs =>
      bufs.in match {
        case List() => (None, bufs)
        case h :: t => (Some(h), bufs.copy(in = t))
      }
    }
    def toReader = ConsoleReader { in => Some(in) }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
    def toReader = ConsoleReader { s => () }  // noop
    def toState = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) }  // append to the output
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)

    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  /** How do we actually _run_ a `ConsoleIO` program? We don't have a `Monad[Console]` for calling
   *  `run`, and we can't use `runTrampoline` either since we have `Console`, not `Function0`. We
   *  need a way to translate from `Console` to `Function0` (if we want to evaluate it sequentially)
   *  or a `Par`.
   *
   *  We introduce the following type to do this translation:
   */

  /** Translate between any `F[A]` to `G[A]`. */
  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }

  type ~>[F[_], G[_]] = Translate[F, G]  // gives us infix syntax `F ~> G` for `Translate[F, G]`

  given function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  given parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] =
      Par.unit(a)

    def flatMap[A, B](a: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(a)(f) }
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)
                            (using G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible, since `step` eliminates these cases")
    }

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  /** The `runConsoleFunction0` implementation is unfortunately not stack safe,
   *  because it relies of the stack safety of the underlying monad, and the
   *  `Function0` monad we gave is not stack safe. To see the problem, try
   *  running: `freeMonad.forever(Console.printLn("Hello"))`.
   */

  // Exercise 4 (optional, hard): Implement `runConsole` using `runFree`,
  // without going through `Par`. Hint: define `translate` using `runFree`.

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[B] = Free[G,B]
    val t = new (F ~> FreeG) {
      def apply[B](a: F[B]): Free[G,B] = Suspend { fg(a) }
    }
    runFree(f)(t)(using freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A =
    runTrampoline {
      translate(a)(
        new (Console ~> Function0) {
          def apply[B](c: Console[B]): () => B = c.toThunk
        }
      )
    }


  /** There is nothing about `Free[Console, A]` that requires we interpret `Console` using side
   *  effects. Here are two pure ways of interpreting a `Free[Console, A]`.
   */
  import Console.*

  case class Buffers(in: List[String], out: Vector[String])

  // A specialized state monad
  case class ConsoleState[A](run: Buffers => (A, Buffers)) {
    infix def map[B](f: A => B): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }

    infix def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
      ConsoleState { s =>
        val (a, s1) = run(s)
        f(a).run(s1)
      }
  }

  object ConsoleState {
    given monad: Monad[ConsoleState] = new Monad[ConsoleState] {
      def unit[A](a: => A) = ConsoleState(bufs => (a,bufs))
      def flatMap[A,B](ra: ConsoleState[A])(f: A => ConsoleState[B]) = ra flatMap f
    }
  }

  // A specialized reader monad
  case class ConsoleReader[A](run: String => A) {
    infix def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))

    infix def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    given monad: Monad[ConsoleReader] = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
    }
  }

  val consoleToState = new (Console ~> ConsoleState) {
    def apply[A](a: Console[A]) = a.toState
  }
  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  /** Can interpet these as before to convert our `ConsoleIO` to a pure value that does no I/O! */
  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](io)(consoleToReader)

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] =
    runFree[Console, ConsoleState, A](io)(consoleToState)

  // So `Free[F, A]` is not really an I/O type. The interpreter `runFree` gets
  // to choose how to interpret these `F` requests, and whether to do "real" I/O
  // or simply convert to some pure value!

  // NB: These interpretations are not stack safe for the same reason,
  // can instead work with `case class ConsoleReader[A](run: String => Trampoline[A])`,
  // which gives us a stack safe monad

  // We conclude that a good representation of an `IO` monad is this:
  type IO[A] = Free[Par, A]

  /** Exercise 5: Implement a non-blocking read from an asynchronous file channel.
   *
   *  We'll just give the basic idea - here, we construct a `Future` by reading
   *  from an `AsynchronousFileChannel`, a `java.nio` class which supports
   *  asynchronous reads.
   */

  import java.nio.*
  import java.nio.channels.*

  // Provides the syntax `Async { k => ... }` for asynchronous IO blocks.
  def Async[A](cb: (A => Unit) => Unit): IO[A] =
    Suspend(Par.async(cb))

  // Provides the `IO { ... }` syntax for synchronous IO blocks.
  def IO[A](a: => A): IO[A] = Suspend { Par.delay(a) }

  def read(file: AsynchronousFileChannel,
           fromPosition: Long,
           numBytes: Int): Par[Either[Throwable, Array[Byte]]] =
    Par.async { (cb: Either[Throwable, Array[Byte]] => Unit) =>
      val buf = ByteBuffer.allocate(numBytes)
      file.read(buf, fromPosition, (), new CompletionHandler[Integer, Unit] {
        def completed(bytesRead: Integer, ignore: Unit): Unit = {
          val arr = new Array[Byte](bytesRead)
          buf.slice.get(arr, 0, bytesRead)
          cb(Right(arr))
        }

        def failed(err: Throwable, ignore: Unit): Unit =
          cb(Left(err))
      })
    }
}

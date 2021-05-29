package fpinscala.iomonad

import fpinscala.iomonad.api.{given, *}
import fpinscala.iomonad.io.{IO2b, IO2c}

import scala.language.implicitConversions

@main def bindTest(): Unit = {

  def timeit(n: Int)(task: => Unit): Unit = {
    val start = System.currentTimeMillis()
    (0 to n).foreach { _ => task }
    val stop = System.currentTimeMillis()
    println(s"${(stop - start) / 1000.0} seconds")
  }

  val N = 100000
  def go[F[_]](m: Monad[F])(unit: F[Unit])(f: F[Int] => Int): Unit = {
    import m.given
    f {
      (0 to N)
        .map(i => m.map(unit)(_ => i))
        .foldLeft(m.unit(0)) { (f1, f2) =>
          for
            acc <- f1
            i   <- f2
          yield {
            // if (i == N) println(s"result: ${acc + i}")
            (acc + i)
          }
        }
    }
  }

  import fpinscala.parallelism.Nonblocking.*

  object ParMonad extends Monad[Par] {
    def unit[A](a: => A) =
      Par.unit(a)

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(pa)(f) }
  }

  val pool = java.util.concurrent.Executors.newFixedThreadPool(4)

  timeit(10) { go(Throw)(Throw.unit(()))(_.run) }
  timeit(10) { go(IO2b.TailRec)(IO2b.TailRec.unit(())) ( IO2b.run ) }
  timeit(10) { go(IO2c.Async)(IO2c.Async.unit(()))(r => Par.run(pool) { IO2c.run(r) }) }
  timeit(10) { go[IO](ioMonad)(ioMonad.unit(()))(r => unsafePerformIO(r)(using pool)) }
  timeit(10) { go(Task)(Task.now(()))(r => r.run(using pool)) }
  timeit(10) { go(Task)(Task.forkUnit(()))(r => r.run(using pool)) }
  timeit(10) { go(ParMonad)(ParMonad.unit(())) { p => Par.run(pool)(p) }}

  // Par.run(pool)(ParMonad.forever { ParMonad.unit { println("woot") }})
  pool.shutdown()
}

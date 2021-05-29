package fpinscala.iomonad.io

import fpinscala.iomonad.Monad

import scala.io.StdIn.readLine
import scala.language.implicitConversions

object IO2b {

  /** As it turns out, there's nothing about this data type that is specific
   *  to I/O, it's just a general purpose data type for optimizing tail calls.
   *  Here it is, renamed to `TailRec`. This type is also sometimes called
   *  `Trampoline`, because of the way interpreting it bounces back and forth
   *  between the main `run` loop and the functions contained in the `TailRec`.
   */

  enum TailRec[A] {
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    infix def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    infix def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] =
      Return(a)

    def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      a flatMap f

    def suspend[A](a: => TailRec[A]): TailRec[A] =
      Suspend(() => ()).flatMap(_ => a)
  }

  import TailRec.*

  @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
    case Return(a)     => a
    case Suspend(r)    => r()
    case FlatMap(x, f) => x match {
      case Return(a)     => run(f(a))
      case Suspend(r)    => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object IO2bTests {
  import IO2b.*

  val f: Int => TailRec[Int] = (i: Int) => TailRec.Return(i)

  val g: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, TailRec[Int]],
       b: Function1[Int, TailRec[Int]]) => {
        (x: Int) => TailRec.suspend(a(x).flatMap(b))
      }
    }

  def main(args: Array[String]): Unit = {
    val gFortyTwo = g(42)
    println("g(42) = " + gFortyTwo)
    println("run(g(42)) = " + run(gFortyTwo))
  }
}

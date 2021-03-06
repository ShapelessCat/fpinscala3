package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.parallelism.*
import fpinscala.state.*
import fpinscala.testing.Gen.*
import fpinscala.testing.Prop.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.language.implicitConversions

/** The library developed in this chapter goes through several iterations. This file is just the
 *  shell, which you can fill in and modify while working through the chapter.
 */

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  infix def map[B](f: A => B): Gen[B] = ???
  infix def flatMap[B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}


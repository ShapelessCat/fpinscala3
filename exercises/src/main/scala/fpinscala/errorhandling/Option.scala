package fpinscala.errorhandling

// Hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Either as _, Option as _, Some as _, *}

enum Option[+A] {
  case Some(get: A)
  case None extends Option[Nothing]

  infix def map[B](f: A => B): Option[B] = ???

  infix def getOrElse[B >: A](default: => B): B = ???

  infix def flatMap[B](f: A => Option[B]): Option[B] = ???

  infix def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???
}

object Option {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 }
    // A `catch` block is just a pattern matching block like the ones we've seen.
    // `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the
    // identifier `e`. The match returns the value 43.

  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else            Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
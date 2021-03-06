package fpinscala.errorhandling

// Hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _, Option as _, None as _, Some as _}

enum Either[+E, +A] {
  case Left(get: E)  extends Either[E, Nothing]
  case Right(get: A) extends Either[Nothing, A]

  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
}

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty
    then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
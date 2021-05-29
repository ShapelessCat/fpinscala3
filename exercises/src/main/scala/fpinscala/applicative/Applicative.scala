package fpinscala
package applicative

import fpinscala.monads.Functor
import fpinscala.monoids.*
import fpinscala.state.State.*
import fpinscala.state.*

import fpinscala.applicative.StateUtil.*

import scala.language.implicitConversions

enum Validation[+E, +A] {
  case Failure(head: E, tail: Vector[E]) extends Validation[E, Nothing]
  case Success(a: A)                     extends Validation[Nothing, A]
}

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A],  fb: F[B])(f: (A,  B) => C): F[C] = ???

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = ???

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???

  infix def product[G[_]](G: Applicative[G]): Applicative[[X] =>> (F[X], G[X])] = ???

  def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[X]]] = ???

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ???
}

object Applicative {

  val lazyListApplicative = new Applicative[LazyList] {

    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a)  // The infinite, constant stream

    override def map2[A, B, C](a: LazyList[A], b: LazyList[B])
                              (f: (A,B) => C): LazyList[C] =  // Combine elements pointwise
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[[X] =>> Validation[E, X]] = ???

  type Const[A, B] = A

  given monoidApplicative[M]: Conversion[Monoid[M], Applicative[[X] =>> Const[M, X]]] =
    (m: Monoid[M]) => new Applicative[[X] =>> Const[M, X]] {
      def unit[A](a: => A): M = m.zero
      override def apply[A, B](m1: M)(m2: M): M = m.op(m1, m2)
    }
}

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))  // TODO: simplify this further???

}

object Monad {
  def eitherMonad[E]: Monad[[X] =>> Either[E, X]] = ???

  def stateMonad[S] = new Monad[[X] =>> State[S, X]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[[X] =>> G[H[X]]] = ???
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(identity)

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative.{given, *}

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[[X] =>> Const[B, X], A, Nothing](as)(f)(using monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[[X] =>> State[S, X], A, B](fa)(f)(using Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa) { (a: A) =>
      for
        s1      <- get[S]
        (b, s2) = f(a, s1)
        _       <- set(s2)
      yield b
    }.run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A]) { (a, s) =>
      ((), a :: s)
    }._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0) { (a, s) =>
      ((a, s), s + 1)
    }._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[M[_], N[_], A, B](fa: F[A])(f: A => M[B], g: A => N[B])
                            (using M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) = ???

  def compose[G[_]](using G: Traverse[G]): Traverse[[X] =>> F[G[X]]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

package fpinscala.parallelism

import java.util.concurrent.*
import scala.language.implicitConversions

type Par[A] = ExecutorService => Future[A]
extension [A](pa: Par[A]) {

  def run(s: ExecutorService): Future[A] =
    pa(s)

  def map[B](f: A => B): Par[B] =
    Par.map2(pa, Par.unit(())) { (a, _) =>
      f(a)
    }

  def choiceMap[B](choices: Map[A, Par[B]]): Par[B] =
    es => {
      val k = pa.run(es).get
      choices(k).run(es)
    }

  def chooser[B](choices: A => Par[B]): Par[B] =
    es => {
      val k = pa.run(es).get
      choices(k).run(es)
    }

  // `chooser` is usually called `flatMap` or `bind`.
  def flatMap[B](choices: A => Par[B]): Par[B] =
    es => {
      val k = pa.run(es).get
      choices(k).run(es)
    }

}

object Par {

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple
  // implementation of `Future` that just wraps a constant value. It doesn't use the
  // `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply
  // returns the value that we gave it.
  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  private final case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design
  // choice of having `fork` be the sole function in the API for controlling parallelism. We can
  // always do `fork(map2(a, b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      // This implementation of `map2` does _not_ respect timeouts. It simply passes the
      // `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and
      // `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts,
      // we'd need a new `Future` implementation that records the amount of time spent evaluating
      // `af`, then subtracts that time from the available time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))
    }

  // This is the simplest and most natural implementation of `fork`, but there are some problems
  // with it -- for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the
  // `ExecutorService`, this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice. This is a symptom of a more
  // serious problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(
      new Callable[A] {
        def call: A = a(es).get
      }
    )

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(Nil)) { (h, t) =>
      map2(h, t)(_ :: _)
    }

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil    => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if as.isEmpty then
      unit(Vector.empty)
    else if as.length == 1 then
      map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l.map(
      asyncF { (a: A) =>
        if f(a) then List(a) else Nil
      }
    )
    map(sequence(pars))(_.flatten)  // convenience method on `List` for concatenating a list of lists
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if cond.run(es).get
      then t(es)  // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = n.run(es).get  // Full source files
      choices(ind).run(es)
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN {
      map(a) { b =>
        if b then 0 else 1
      }
    }(List(ifTrue, ifFalse))

  def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    flatMap(p) { b =>
      if b then t else f
    }

  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(choices)

  // see nonblocking implementation in `Nonblocking.scala`
  def join[A](a: Par[Par[A]]): Par[A] =
    es => a.run(es).get().run(es)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))
}

object Examples {
  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library.
  // Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two
  // parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if ints.size <= 1 then
      // `headOption` is a method defined on all collections in Scala. We saw this function
      // in chapter 3.
      ints.headOption getOrElse 0
    else {
      // Divide the sequence in half using the `splitAt` function.
      val (l, r) = ints.splitAt(ints.length/2)
      // Recursively sum both halves and add the results together.
      sum(l) + sum(r)
    }
}

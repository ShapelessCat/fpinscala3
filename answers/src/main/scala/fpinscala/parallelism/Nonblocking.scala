package fpinscala.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import scala.language.implicitConversions

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    extension [A](p: Par[A])
      infix def map2[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
        es => new Future[C] {
          def apply(cb: C => Unit): Unit = {
            var ar = Option.empty[A]
            var br = Option.empty[B]
            // this implementation is a little too liberal in forking of threads --
            // it forks a new logical thread for the actor and for stack-safety,
            // forks evaluation of the callback `cb`
            val combiner = Actor[Either[A, B]](es) {
              case Left(a) =>
                if br.isDefined
                then eval(es)(cb(f(a, br.get)))
                else ar = Some(a)

              case Right(b) =>
                if ar.isDefined
                then eval(es)(cb(f(ar.get, b)))
                else br = Some(b)
            }
            p(es)(a => combiner ! Left(a))
            p2(es)(b => combiner ! Right(b))
          }
        }

      // specialized version of `map`
      infix def map[B](f: A => B): Par[B] =
        es => new Future[B] {
          def apply(cb: B => Unit): Unit =
            p(es)(a => eval(es) { cb(f(a)) })
        }

      def flatMap[B](f: A => Par[B]): Par[B] =
        es => new Future[B] {
          def apply(cb: B => Unit): Unit =
            p(es)(a => f(a)(es)(cb))
        }

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      // A mutable, threadsafe reference, to use for storing the result
      val ref = new java.util.concurrent.atomic.AtomicReference[A]
      // A latch which, when decremented, implies that `ref` has the result
      val latch = new CountDownLatch(1)
      // Asynchronously set the result, and decrement the latch
      p(es) { a =>
        ref.set(a)
        latch.countDown()
      }
      // Block until the `latch.countDown` is invoked asynchronously
      latch.await()
      // Once we've passed the latch, we know `ref` has been set, and return its value
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] =
      es => new Future[A] {
        def apply(k: A => Unit): Unit = f(k)
      }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit {
        new Callable[Unit] {
          def call(): Unit = r
        }
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil    => unit(Nil)
        case h :: t => h.map2(fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if as.isEmpty then
        unit(Vector.empty)
      else if as.length == 1 then
        map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length/2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      sequenceBalanced(as.toIndexedSeq).map(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    // exercise answers

    /** We can implement `choice` as a new primitive.
     *
     *  `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     *  some `Par`, `p`, is the idiom for running `p`, and registering
     *  a callback to be invoked when its result is available. The
     *  result will be bound to `result` in the function passed to
     *  `p(es)`.
     *
     *  If you find this code difficult to follow, you may want to
     *  write down the type of each subexpression and follow the types
     *  through the implementation. What is the type of `p(es)`? What
     *  about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            eval(es) { (if b then t else f)(es)(cb) }
          }
      }

    // The code here is very similar.
    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { ind =>
            eval(es) { ps(ind)(es)(cb) }
          }
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if b then 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit): Unit =
          p(es)(k => ps(k)(es)(cb))
      }

    //`chooser` is usually called `flatMap` or `bind`.
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      flatMap(p)(f)

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if b then t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es)(p2 => eval(es) { p2(es)(cb) })
      }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(identity)

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))
  }
}

package fpinscala.testing.exhaustive

/** This source file contains the answers to the last two exercises in the section
 *  "Test Case Minimization" of chapter 8 on property-based testing.
 *
 *  The Gen data type in this file incorporates exhaustive checking of finite domains.
 */

import fpinscala.laziness.LazyList
import fpinscala.laziness.LazyList.*
import fpinscala.parallelism.*
import fpinscala.state.*
import fpinscala.testing.exhaustive.Gen.*
import fpinscala.testing.exhaustive.Prop.*
import fpinscala.testing.exhaustive.Status.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.language.implicitConversions

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Right((_, n)) => p.run(max, n, rng).map { case (s, m) => (s, n+m) }
      case l             => l
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Left(msg) => p.tag(msg).run(max, n, rng)
      case r         => r
    }
  }

  /** This is rather simplistic - in the event of failure, we simply prepend
   *  the given message on a newline in front of the existing message.
   */
  infix def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Left(e) => Left(msg + "\n" + e)
      case r       => r
    }
  }
}

object Prop {
  // TODO; use opaque types???
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String
  type Result = Either[FailedCase, (Status, TestCases)]
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => {
      def go(i: Int, j: Int, s: LazyList[Option[A]], onEnd: Int => Result): Result =
        if i == j then
          Right((Unfalsified, i))
        else s match {
          case Cons(h, t) => h() match {
            case Some(h) =>
              try {
                if f(h)
                then go(i+1, j, t(), onEnd)
                else Left(h.toString) }
              catch { case e: Exception => Left(buildMsg(h, e)) }
            case None => Right((Unfalsified, i))
          }
          case _ => onEnd(i)
        }

      go(0, n/3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n/3, n, rands, i => Right((Unfalsified, i)))
        case s => s  // If proven or failed, stop immediately
      }
    }
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  /* We pattern match on the `SGen`, and delegate to our `Gen` version of `forAll`
   * if `g` is unsized; otherwise, we call the sized version of `forAll` (below).
   */
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Unsized(g2) => forAll(g2)(f)
    case Sized(gs)   => forAll(gs)(f)
  }

  /* The sized case of `forAll` is as before, though we convert from `Proven` to
   * `Exhausted`. A sized generator can never be proven, since there are always
   * larger-sized tests that were not run which may have failed.
   */
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = n / max + 1
      val props: List[Prop] = LazyList.from(0).take(max+1).map(i => forAll(g(i))(f)).toList
      val p: Prop = props.map { p =>
        Prop((max, n, rng) => p.run(max, casesPerSize, rng))
      }.reduceLeft(_ && _)

      p.run(max, n, rng).map {
        case (Proven,n) => (Exhausted,n)
        case x          => x
      }
  }

  def run(p: Prop,
          maxSize: Int = 100, // A default argument of `200`
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Left(msg)               => println("! test failed:\n" + msg)
      case Right((Unfalsified, n)) => println("+ property unfalsified, ran " + n + " tests")
      case Right((Proven, n))      => println("+ property proven, ran " + n + " tests")
      case Right((Exhausted, n))   => println("+ property unfalsified up to max size, ran " + n + " tests")
    }
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1))) { i =>
    i.map(_ + 1)(ES).get == Par.unit(2)(ES).get
  }

  def check(p: => Boolean): Prop = // Note that we are non-strict here
    forAll(unit(()))(_ => p)

  val p2 = check {
    val p = Par.unit(1).map(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal (
      Par.unit(1).map(_ + 1),
      Par.unit(2)
    ) (ES).get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  val pint = Gen.choose(0,10).map(Par.unit)
  val p4 = forAllPar(pint)(n => equal(n.map(identity), n))

  val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
}

enum Status {
  case Exhausted
  case Proven
  case Unfalsified
}

/** The `Gen` type now has a random generator as well as an exhaustive stream.
 *  Infinite domains will simply generate infinite streams of None.
 *  A finite domain is exhausted when the stream reaches empty.
*/
case class Gen[+A](sample: State[RNG, A], exhaustive: LazyList[Option[A]]) {
  infix def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f),
        map2Stream(exhaustive, g.exhaustive)(map2Option(_, _)(f)))

  infix def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(
      sample.flatMap(a => f(a).sample),
      exhaustive.flatMap {
        case None    => unbounded
        case Some(a) => f(a).exhaustive
      }
    )

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)
  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def unsized: Unsized[A] = Unsized(this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))
}

object Gen {
  type Domain[+A] = LazyList[Option[A]]

  def bounded[A](a: LazyList[A]): Domain[A] = a.map(Some(_))
  def unbounded: Domain[Nothing] = LazyList(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(LazyList(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean), bounded(LazyList(true, false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)),
        bounded(LazyList.from(start).take(stopExclusive-start)))

  /* This implementation is rather tricky, but almost impossible to get wrong
   * if you follow the types. It relies on several helper functions (see below).
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)),
        cartesian(LazyList.continually(g.exhaustive).take(n)).map(l => sequenceOption(l.toList)))

  /* `cartesian` generates all possible combinations of a `Stream[Stream[A]]`. For instance:
   *
   *    cartesian(Stream(Stream(1,2), Stream(3), Stream(4,5))) ==
   *    Stream(Stream(1,3,4), Stream(1,3,5), Stream(2,3,4), Stream(2,3,5))
  */
  def cartesian[A](s: LazyList[LazyList[A]]): LazyList[LazyList[A]] =
    s.foldRight(LazyList(LazyList[A]()))((hs, ts) => map2Stream(hs,ts)(LazyList.cons(_, _)))

  /* `map2Option` and `map2Stream`. Notice the duplication! */
  def map2Option[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- oa
      b <- ob
    yield f(a, b)

  /* This is not the same as `zipWith`, a function we've implemented before.
   * We are generating all (A, B) combinations and using each to produce a `C`.
   * This implementation desugars to sa.flatMap(a => sb.map(b => f(a,b))).
   */
  def map2Stream[A, B, C](sa: LazyList[A], sb: => LazyList[B])(f: (A, => B) => C): LazyList[C] =
    for
      a <- sa
      b <- sb
    yield f(a, b)

  /* This is a function we've implemented before. Unfortunately, it does not
   * exist in the standard library. This implementation is uses a foldLeft,
   * followed by a reverse, which is equivalent to a foldRight, but does not
   * use any stack space.
   */
  def sequenceOption[A](o: List[Option[A]]): Option[List[A]] =
    o.foldLeft[Option[List[A]]](Some(Nil)) {
      (t, h) => map2Option(h, t)(_ :: _)
    }.map(_.reverse)

  /* Notice we are using the `unbounded` definition here, which is just
   * `Stream(None)` in our current representation of `exhaustive`.
   */
  def uniform: Gen[Double] =
    Gen(State(RNG.double), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d*(j-i)), unbounded)

  /** Basic idea is add 1 to the result of `choose` if it is of the wrong
   *  parity, but we require some special handling to deal with the maximum
   *  integer in the range.
   */
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if stopExclusive%2 == 0 then stopExclusive - 1 else stopExclusive).
    map (n => if n%2 != 0 then n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if stopExclusive%2 != 0 then stopExclusive - 1 else stopExclusive).
    map (n => if n%2 == 0 then n+1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for
    i <- choose(from, to)
    j <- if i%2 == 0 then even(from, to) else odd(from, to)
  yield (i, j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List.empty[A])) { (a,b) =>
      a.map2(b)(_ :: _)
    }

  /** The simplest possible implementation. This will put all elements of one
   *  `Gen` before the other in the exhaustive traversal. It might be nice to
   *  interleave the two streams, so we get a more representative sample if we
   *  don't get to examine the entire exhaustive stream.
   */
  def union_1[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(
      State(RNG.boolean).flatMap(b => if b then g1.sample else g2.sample),
      interleave(g1.exhaustive, g2.exhaustive)
    )

  def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    s1.zipAll(s2).flatMap { case (a, a2) => LazyList((a.toList ++ a2.toList)*) }

  /* The random case is simple - we generate a double and use this to choose between
   * the two random samplers. The exhaustive case is trickier if we want to try
   * to produce a stream that does a weighted interleave of the two exhaustive streams.
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    /* Some random booleans to use for selecting between g1 and g2 in the exhaustive case.
     * Making up a seed locally is fine here, since we just want a deterministic schedule
     * with the right distribution. */
    def bools: LazyList[Boolean] = randomStream(uniform.map(_ < g1Threshold))(RNG.Simple(302837L))

    Gen(State(RNG.double).flatMap(d => if d < g1Threshold then g1._1.sample else g2._1.sample),
        interleave(bools, g1._1.exhaustive, g2._1.exhaustive))
  }

  /** Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  /** Interleave the two streams, using `b` to control which stream to pull from at each step.
   *  A value of `true` attempts to pull from `s1`; `false` attempts to pull from `s1`.
   *  When either stream is exhausted, insert all remaining elements from the other stream.
   */
  def interleave[A](b: LazyList[Boolean], s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    b.headOption map { hd =>
      if hd then s1 match {
        case Cons(h, t) => LazyList.cons(h(), interleave(b drop 1, t(), s2))
        case _          => s2
      }
      else s2 match {
        case Cons(h, t) => LazyList.cons(h(), interleave(b drop 1, s1, t()))
        case _          => s1
      }
    } getOrElse LazyList.empty

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    Sized(n => g.listOfN(n))

  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  def string: SGen[String] = Sized(stringN)

  case class Sized[+A](forSize: Int => Gen[A]) extends SGen[A]
  case class Unsized[+A](get: Gen[A]) extends SGen[A]

  given unsized[A]: Conversion[Gen[A], SGen[A]] =
    Unsized(_)

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    Sized(n => g.listOfN(n max 1))

  val maxProp1 = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !l.zip(ls.tail).exists { case (a, b) => a > b }
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  /** A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
   *  computation for each element of the input list summed to produce the final
   *  result. This is not the most compelling example, but it provides at least some
   *  variation in structure to use for testing.
   */
  lazy val pint2: Gen[Par[Int]] =
    choose(-100,100)
      .listOfN(choose(0,20))
      .map { l =>
        l.foldLeft(Par.unit(0)) { (p, i) =>
          Par.fork { Par.map2(p, Par.unit(i))(_ + _) }
        }
      }

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => (s => i))
}

trait SGen[+A] {
  infix def map[B](f: A => B): SGen[B] = this match {
    case Sized(g)   => Sized(g andThen (_ map f))
    case Unsized(g) => Unsized(g map f)
  }

  infix def flatMap[B](f: A => Gen[B]): SGen[B] = this match {
    case Sized(g)   => Sized(g andThen (_ flatMap f))
    case Unsized(g) => Unsized(g flatMap f)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = (this,s2) match {
    case (Sized(g), Sized(g2))     => Sized(n => g(n) ** g2(n))
    case (Unsized(g), Unsized(g2)) => Unsized(g ** g2)
    case (Sized(g), Unsized(g2))   => Sized(n => g(n) ** g2)
    case (Unsized(g), Sized(g2))   => Sized(n => g ** g2(n))
  }
}

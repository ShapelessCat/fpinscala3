package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.*
import fpinscala.parallelism.Nonblocking.Par.*  // infix extension methods for `Par.map`, `Par.flatMap`, etc
import scala.language.implicitConversions

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x + y
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x * y
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x || y
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x && y
    val zero = true
  }

  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations satisfy the monoid
  // laws, but they are not equivalent.
  //   This is true in general -- that is, every monoid has a _dual_ where the `op` combines things
  // in the opposite order. Monoids like `booleanOr` and `intAddition` are equivalent to their duals
  // because their `op` is commutative as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]): Option[A] = x orElse y
    val zero = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  import fpinscala.testing.*
  import Prop.*

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    // Associativity
    forAll(
      for
        x <- gen
        y <- gen
        z <- gen
      yield (x, y, z)
    ) { (a, b, c) =>
      m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
    } &&
      forAll(gen) { (a: A) =>  // Identity
        m.op(a, m.zero) == a && m.op(m.zero, a) == a
      }

  def concatenate[A](xs: List[A], m: Monoid[A]): A =
    xs.foldLeft(m.zero)(m.op)

  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  def foldMap[A, B](xs: List[A], m: Monoid[B])(f: A => B): B =
    xs.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](xs: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(xs, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to the function `f` to put the `B`
  // on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](xs: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(xs, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](xs: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if xs.isEmpty then
      m.zero
    else if xs.length == 1 then
      f(xs(0))
    else {
      val (l, r) = xs.splitAt(xs.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // This implementation detects only ascending order, but you can write a monoid that detects both
  // ascending and descending order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some(x1, y1, p), Some(x2, y2, q)) => Some(x1 min x2, y1 max y2, p && q && y1 <= x2)
          case (x, None)                          => x
          case (None, x)                          => x
        }
      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some(i, i, true)).forall(_._3)
  }

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]) = a.map2(b)(m.op)
  }

  // we perform the mapping and the reducing both in parallel
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  enum WC {
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)
  }

  import WC.{Stub, Part}

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d))                   => Stub(c + d)
      case (Stub(c), Part(l, w, r))             => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c))             => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    // A single character's count. Whitespace does not count, and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if c.isWhitespace
      then Part("", 0, "")
      else Stub(c.toString)

    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s)       => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) =
        (A.op(x._1, y._1), B.op(x._2, y._2))

      val zero: (A, B) =
        (A.zero, B.zero)
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B): A => B =
        a => B.op(f(a), g(a))

      val zero: A => B =
        a => B.zero
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map.empty[K, V]

      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
    }


  def bag[A](xs: IndexedSeq[A]): Map[A, Int] =
    foldMapV(xs, mapMergeMonoid[A, Int](intAddition)) { (a: A) =>
      Map(a -> 1)
    }

}

trait Foldable[F[_]] {
  import Monoid.*

  def foldRight[A, B](xs: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(xs)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](xs: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(xs) { a =>
      (b: B) => f(b, a)
    }(dual(endoMonoid[B]))(z)

  def foldMap[A, B](xs: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(xs)(mb.zero) { (a, b) =>
      mb.op(f(a), b)
    }

  def concatenate[A](xs: F[A])(m: Monoid[A]): A =
    foldLeft(xs)(m.zero)(m.op)

  def toList[A](xs: F[A]): List[A] =
    foldRight(xs)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](xs: List[A])(z: B)(f: (A, B) => B): B =
    xs.foldRight(z)(f)

  override def foldLeft[A, B](xs: List[A])(z: B)(f: (B, A) => B): B =
    xs.foldLeft(z)(f)

  override def foldMap[A, B](xs: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(xs)(mb.zero) { (b, a) =>
      mb.op(b, f(a))
    }

  override def toList[A](xs: List[A]): List[A] =
    xs
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid.*

  override def foldRight[A, B](xs: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    xs.foldRight(z)(f)

  override def foldLeft[A, B](xs: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    xs.foldLeft(z)(f)

  override def foldMap[A, B](xs: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(xs, mb)(f)
}

object LazyListFoldable extends Foldable[LazyList] {
  override def foldRight[A, B](xs: LazyList[A])(z: B)(f: (A, B) => B): B =
    xs.foldRight(z)(f)

  override def foldLeft[A, B](xs: LazyList[A])(z: B)(f: (B, A) => B): B =
    xs.foldLeft(z)(f)
}

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])
}

object TreeFoldable extends Foldable[Tree] {
  import Tree.{Leaf, Branch}

  override def foldMap[A, B](xs: Tree[A])(f: A => B)(mb: Monoid[B]): B = xs match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](xs: Tree[A])(z: B)(f: (B, A) => B): B = xs match {
    case Leaf(a)      => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](xs: Tree[A])(z: B)(f: (A, B) => B): B = xs match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

// Notice that in `TreeFoldable.foldMap`, we don't actually use the `zero`
// from the `Monoid`. This is because there is no empty tree.
// This suggests that there might be a class of types that are foldable
// with something "smaller" than a monoid, consisting only of an
// associative `op`. That kind of object (a monoid without a `zero`) is
// called a semigroup. `Tree` itself is not a monoid, but it is a semigroup.

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](xs: Option[A])(f: A => B)(mb: Monoid[B]): B =
    xs match {
      case None    => mb.zero
      case Some(a) => f(a)
    }

  override def foldLeft[A, B](xs: Option[A])(z: B)(f: (B, A) => B): B = xs match {
    case None    => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](xs: Option[A])(z: B)(f: (A, B) => B): B = xs match {
    case None    => z
    case Some(a) => f(a, z)
  }
}


package fpinscala.laziness

enum Stream[+A] {
  case Empty extends Stream[Nothing]
  case Cons(h: () => A, t: () => Stream[A])

  import Stream.*

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second
  // argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)`
    // returns `true`, `b` will never be evaluated and the computation terminates early.
    foldRight(false) { (a, b) =>
      p(a) || b
    }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)
  }

  def toListNaive: List[A] = this match {
    case Empty            => Nil
    case Cons(head, tail) => head() :: tail().toListNaive
  }

  def toListTailRec: List[A] = {
    @annotation.tailrec
    def aux(cur: Stream[A], acc: List[A]): List[A] =
      cur match {
        case Empty      => acc.reverse
        case Cons(h, t) => aux(t(), h() :: acc)
      }
    aux(this, Nil)
  }

  def toList: List[A] = {
    val acc = collection.mutable.ListBuffer.empty[A]
    @annotation.tailrec
    def aux(cur: Stream[A]): List[A] =
      cur match {
        case Empty      => acc.toList
        case Cons(h, t) => acc += h(); aux(t())
      }
    aux(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0  => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) =>
//      lazy val hv = h()
//      if p(hv) then cons(hv, t().takeWhile(p)) else Empty
//
//    case Empty => Empty
//  }

  // EXERCISE 5.5
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, acc) =>
      if p(h) then cons(h, acc) else acc
    }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true) { (h, acc) =>
      p(h) && acc
    }

  def headOption: Option[A] =
    this.foldRight(Option.empty[A]) { (h, _) =>
      Some(h)
    }

  // 5.7 map, filter, append, flatmap using foldRight.
  // Part of the exercise is writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (h, acc) =>
      cons(f(h), acc)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, acc) =>
      if p(h) then cons(h, acc) else acc
    }

  def append[B >: A](e: => B): Stream[B] =
    foldRight(empty[B]) {
      case (h, acc: Cons[B]) => cons(h, acc)
      case (h, Empty)        => cons(h, cons(e, Empty))
    }

  def appendAll[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s) { (h, acc) => cons(h, acc) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (h, acc) =>
      f(h).appendAll(acc)
    }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty      => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some(h(), (empty, 0))
      case (Cons(h, t), m) if m > 1 => Some(h(), (t(), m - 1))
      case _                        => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case (Empty, _) | (_, Empty)      => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1(), t2()))
      case (Cons(h1, t1), Empty)        => Some((Some(h1()), None)       -> (t1(), Empty))
      case (Empty, Cons(h2, t2))        => Some((None, Some(h2()))       -> (Empty, t2()))
      case (Empty, Empty)               => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll {
      case (h1, h2) => h1 == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case c @ Cons(h, t) => Some(c, t())
      case Empty          => None
    }.append(empty[A])

  // TODO: Re-write
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    ???
  // TODO: Compare this with the given answer, and find
  //       out the effiency issue. It seem my solution
  //       evaluate each Result element twice, while the
  //       solution evaluate only once.
//    foldRight(empty[B]) {
//      case (e, s @ Cons(h, _)) => cons(f(e, h()), s)
//      case (e, Empty)          => cons(f(e, z), Stream(z))
//    }

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if as.isEmpty
    then empty
    else cons(as.head, apply(as.tail*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val r: Stream[A] = cons(a, r)
    r
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fib: Stream[Int] = {
    def aux(a: Int, b: Int): Stream[Int] =
      cons(a, cons(b, aux(b, a + b)))

    aux(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some(a, s) => cons(a, unfold(s)(f))
      case None       => Empty
    }

  val onesViaUnfold: Stream[1] = unfold(1)(Function.const(Some(1, 1)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(Function.const(Some(a, a)))
  }

  def fibViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (a, b) =>
      Some(a, (b, a + b))
    }

}
package fpinscala.laziness

enum LazyList[+A] {
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _          => Nil
  }

  /** The above solution will stack overflow for large streams, since it's not tail-recursive. Here
   *  is a tail-recursive implementation. At each step we cons onto the front of the `acc` list,
   *  which will result in the reverse of the stream. Then at the end we reverse the result to get
   *  the correct order again.
   */
  def toList: List[A] = {
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc.reverse
    }
    go(this, Nil)
  }

  /** In order to avoid the `reverse` at the end, we could write it using a mutable list buffer and
   *  an explicit loop instead. Note that the mutable list buffer never escapes our `toList` method,
   *  so this function is still _pure_.
   */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(ll: LazyList[A]): List[A] = ll match {
      case Cons(h, t) =>
        buf += h()
        go(t())

      case _ => buf.toList
    }
    go(this)
  }

  /** Create a new Stream[A] from taking the n first elements from this. We can achieve that by
   *  recursively calling take on the invoked tail of a cons cell. We make sure that the tail is not
   *  invoked unless we need to, by handling the special case where n == 1 separately. If n == 0, we
   *  can avoid looking at the stream at all.
   */
  def take(n: Int): LazyList[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  /** Create a new Stream[A] from this, but ignore the n first elements. This can be achieved by
   *  recursively calling drop on the invoked tail of a cons cell. Note that the implementation is
   *  also tail recursive.
   */
  @annotation.tailrec
  final infix def drop(n: Int): LazyList[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  /** It's a common Scala style to write method calls without `.` notation,
   *  as in `t() takeWhile f`.
   */
  def takeWhile(f: A => Boolean): LazyList[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _                    => empty
  }

  // The arrow `=>` in front of the argument type `B` means that the function
  // `f` takes its second argument by name and may choose not to evaluate it.
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

  /** Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a
   *  nonmatching element is found.
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) =>
      p(a) && b
    }

  def takeWhile_1(p: A => Boolean): LazyList[A] =
    foldRight(empty[A]) { (h, t) =>
      if p(h)
      then cons(h, t)
      else empty
    }

  def headOption: Option[A] =
    foldRight(Option.empty[A]) { (h, _) =>
      Some(h)
    }

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B]) { (a, acc) =>
      cons(f(a), acc)
    }

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty[A]) { (a, acc) =>
      if p(a)
      then cons(a, acc)
      else acc
    }

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that) { (a, acc) =>
      cons(a, acc)
    }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B]) { (a, acc) =>
      f(a).append(acc)
    }

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _          => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some(h(), (empty, 0))
      case (Cons(h, t), m) if n > 1 => Some(h(), (t(), m - 1))
      case _                        => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _                    => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty)               => None
      case (Cons(h1, t1), Empty)        => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2))        => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
    }

  def zipWith[B, C](s2: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  // special case of `zipWith`
  def zip[B](s2: LazyList[B]): LazyList[(A, B)] =
    zipWith(s2)((_, _))

  def zipWithAll[B, C](s2: LazyList[B])(f: (Option[A], Option[B]) => C): LazyList[C] =
    LazyList.unfold((this, s2)) {
      case (Empty, Empty)               => None
      case (Cons(h, t), Empty)          => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t))          => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAllViaZipWithAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    zipWithAll(that)((_, _))

  /** `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point
   *  that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match,
   *  we terminate early. Using non-strictness, we can compose these three separate logical steps
   *  -- the zipping, the termination when the second stream is exhausted, and the termination if a
   *  nonmatching element is found or the first stream is exhausted.
   */
  def startsWith[B](prefix: LazyList[B]): Boolean =
    zipAll(prefix).takeWhile(_._2.isDefined) forAll {
      case (a1, a2) => a1 == a2
    }

  /** The last element of `tails` is always the empty `Stream`, so we handle this as a special case,
   *  by appending it to the output.
   */
  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Empty          => None
      case l @ Cons(_, t) => Some(l, t())
    }.append(LazyList(empty))

  def hasSubsequence[B](s: LazyList[B]): Boolean =
    tails.exists(_.startsWith(s))

  /** The function can't be implemented using `unfold`, since `unfold` generates elements of the
   *  `Stream` from left to right. It can be implemented using `foldRight` though.
   *
   *  The implementation is just a `foldRight` that keeps the accumulated value and the stream of
   *  intermediate results, which we `cons` onto during each iteration. When writing folds, it's
   *  common to have more state in the fold than is needed to compute the result. Here, we simply
   *  extract the accumulated list once finished.
   */
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight((init, LazyList(init))) { (a, b0) =>
      // `b0` is passed by-name and used in by-name args in `f` and `cons`.
      // So use `lazy val` to ensure only one evaluation...
      lazy val (b, sb) = b0
      val b2 = f(a, b)
      (b2, cons(b2, sb))
    }._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)
  }
}

object LazyList {
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty
    then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  // This is more efficient than `cons(a, constant(a))`
  // since it's just one object referencing itself.
  def continually[A](a: A): LazyList[A] = {
    lazy val single: LazyList[A] = cons(a, single)
    single
  }

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  val fibs: LazyList[Int] = {
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))

    go(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match {
      case Some(h, s) => cons(h, unfold(s)(f))
      case None       => empty
    }

  // The below two implementations use `fold` and `map` functions in the Option class to implement
  // unfold, thereby doing away with the need to manually pattern match as in the above solution.
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).fold(empty[A]) { (a, s) =>
      cons(a, unfold(s)(f))
    }

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).map { (a, s) =>
      cons(a, unfold(s)(f))
    }.getOrElse(empty[A])

  // Scala provides shorter syntax when the first action of a function literal is to match on an
  // expression. The function passed to `unfold` in `fibsViaUnfold` is equivalent to
  // `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to
  // pattern match on it.
  val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { (current, next) =>
      Some(current, (next, current + next))
    }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ()))

  // could also of course be implemented as constant(1)
  val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))
}

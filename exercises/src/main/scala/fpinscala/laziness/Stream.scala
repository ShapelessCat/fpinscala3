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
  def take(n: Int): Stream[A] = ???

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = ???

  def forAll(p: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight.
  // Part of the exercise is writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
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

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
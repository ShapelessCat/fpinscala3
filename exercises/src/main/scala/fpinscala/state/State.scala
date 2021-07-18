package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)  // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = Simple(newSeed)
      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i0, rng1) = rng.nextInt
    (if i0 >= 0 then i0 else -(i0 + 1), rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1)) -> r
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i0, rng1) = rng.nextInt
    val (d0, rng2) = double(rng1)
    (i0, d0) -> rng2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    (d, i) -> r
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d0, rng1) = double(rng)
    val (d1, rng2) = double(rng1)
    val (d2, rng3) = double(rng2)
    (d0, d1, d2) -> rng3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def aux(cnt: Int, acc: List[Int], r: RNG): (List[Int], RNG) =
      if cnt <= 0
      then acc -> r
      else {
        val (i, nr) = r.nextInt
        aux(cnt - 1, i :: acc, nr)
      }

    aux(count, Nil, rng)
  }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r0) = ra(rng)
      val (b, r1) = rb(r0)
      f(a, b) -> r1
    }

  def sequenceVerbose[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      fs.foldRight((List.empty[A], rng)) {
        case (f, (acc, cr)) =>
          val (v, nr) = f(cr)
          (v :: acc) -> nr
      }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) {
      (f, acc) => map2(f, acc)(_ :: _)
    }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  infix def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  infix def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

enum Input {
  case Coin
  case Turn
}

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

def takeWhile_1(f: A => Boolean): Stream[A] =
  foldRight(empty[A]){ (h, t) =>
    if f(h)
    then cons(h, t)
    else empty
  }

def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
  p.flatMap(f andThen succeed)

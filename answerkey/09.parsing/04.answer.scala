def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
  if n <= 0
  then succeed(Nil)
  else map2(p, listOfN(n-1, p))(_ :: _)

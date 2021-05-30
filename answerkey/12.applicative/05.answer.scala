def eitherMonad[E]: Monad[[X] =>> Either[E, X]] =
  new Monad[[X] =>> Either[E, X]] {
    def unit[A](a: => A): Either[E, A] = Right(a)

    def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }
  }

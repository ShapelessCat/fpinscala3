def freeMonad[F[_]]: Monad[[A] = Free[F, A]] =
  new Monad[[A] = Free[F, A]] {
    def unit[A](a: => A) =
      Return(a)

    def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]) =
      fa flatMap f
  }

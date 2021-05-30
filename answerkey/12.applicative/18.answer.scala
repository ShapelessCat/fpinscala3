def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                          (using G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
  traverse[[X] =>> (G[X], H[X]), A, B](fa)(a => (f(a), g(a)))(G product H)

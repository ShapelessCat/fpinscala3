def compose[G[_]](using G: Traverse[G]): Traverse[[X] =>> F[G[X]]] =
  new Traverse[[X] =>> F[G[X]]] {
    override def traverse[M[_]: Applicative, A, B](fa: F[G[A]])(f: A => M[B]) =
      self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
  }

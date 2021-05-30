def compose[G[_]](G: Applicative[G]): Applicative[[X] =>> F[G[x]]] = {
  val self = this

  new Applicative[[X] =>> F[G[x]]] {
    def unit[A](a: => A) = self.unit(G.unit(a))

    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
      self.map2(fga, fgb)(G.map2(_, _)(f))
  }
}

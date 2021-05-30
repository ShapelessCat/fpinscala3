def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[[X] =>> G[H[X]]] =
  new Monad[[X] =>> G[H[X]]] {
    def unit[A](a: => A): G[H[A]] =
      G.unit(H.unit(a))

    override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
  }

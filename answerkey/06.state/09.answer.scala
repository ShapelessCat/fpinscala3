def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(f andThen unit)

def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra){ a =>
    map(rb)(b => f(a, b))
  }

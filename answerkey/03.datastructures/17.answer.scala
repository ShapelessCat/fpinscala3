def doubleToString(l: List[Double]): List[String] = 
  foldRight(l, List.empty[String]) { (h, t) =>
    Cons(h.toString, t)
  }

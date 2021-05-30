def add1(l: List[Int]): List[Int] = 
  foldRight(l, List.empty[Int]) { (h, t) =>
    Cons(h+1, t)
  }

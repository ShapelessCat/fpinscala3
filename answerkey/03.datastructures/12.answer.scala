def reverse[A](l: List[A]): List[A] =
  foldLeft(l, List.empty[A]) { (acc, h) =>
    Cons(h, acc)
  }

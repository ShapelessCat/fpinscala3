def toList[A](as: F[A]): List[A] =
  foldRight(as)(List.empty[A])(_ :: _)

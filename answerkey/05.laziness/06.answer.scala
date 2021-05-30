def headOption: Option[A] =
  foldRight(Option.empty[A]) { (h, _) =>
    Some(h)
  }

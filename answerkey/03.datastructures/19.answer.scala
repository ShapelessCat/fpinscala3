// The discussion about `map` also applies here.
def filter[A](l: List[A])(f: A => Boolean): List[A] =
  foldRight(l, List.empty[A]) { (h, t) =>
    if f(h) then Cons(h, t) else t
  }

def filter_1[A](l: List[A])(f: A => Boolean): List[A] = 
  foldRightViaFoldLeft(l, List.empty[A]){ (h,t) =>
    if f(h) then Cons(h, t) else t
  }

def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
  val buf = new collection.mutable.ListBuffer[A]
  def go(l: List[A]): Unit = l match {
    case Nil        => ()
    case Cons(h, t) => if f(h) then buf += h; go(t)
  }
  go(l)
  // converting from the standard Scala list to the list we've defined here
  List(buf.toList*)
}
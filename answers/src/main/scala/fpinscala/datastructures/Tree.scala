package fpinscala.datastructures

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  /** Again, note how similar the implementation is to `size` and `maximum`. */
  def depth: Int = this match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (l.depth max r.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  /** Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of
   *  the type, and recursively accumulates some value using these handlers. As with `foldRight`,
   *  `fold(t)(Leaf(_))(Branch(_, _)) == t`, and we can use this function to implement just about
   *  any recursive function that would otherwise be defined by pattern matching.
   */
  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }

  def sizeViaFold: Int =
    fold(a => 1)(1 + _ + _)

  def depthViaFold: Int =
    fold(a => 0) { (d1, d2) =>
      1 + (d1 max d2)
    }

  /** Note:
   *  In Scala 2, the type annotation is required on the expression `Leaf(f(a))`.
   *  Without this annotation, we get an error like this:
   *
   *  type mismatch;
   *    found   : fpinscala.datastructures.Branch[B]
   *    required: fpinscala.datastructures.Leaf[B]
   *       fold(t)(a => Leaf(f(a)))(Branch(_, _))
   *                                      ^
   *  In Scala 3, it not use `enum` to define the `Tree`, this type annotation is also required.
   *
   *  Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it
   *  is then expected that the second argument to `fold` will return `Leaf[B]`, which it doesn't
   *  (it returns `Branch[B]`). Really, we'd prefer Scala to infer `Tree[B]` as the result type in
   *  both cases. When working with algebraic data types in Scala, it's somewhat common to define
   *  helper functions that simply call the corresponding data constructors but give the less
   *  specific result type:
   *
   *    def leaf[A](a: A): Tree[A] = Leaf(a)
   *    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
   *
   *  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data
   *  types. This issue is partially fixed in Scala 3 because of the introduction of `enum`, and
   *  the type inference of `enum` variants, though this issue still exists if you don't use `enum`.
   */
  def mapViaFold[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)))(Branch(_, _))
}

object Tree {

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match {
    case Leaf(i)      => Option.when(i > 0)(i)
    case Branch(l, r) => l.firstPositive orElse r.firstPositive
  }

  /** We're using the method `max` that exists on all `Int` values rather than an explicit `if`
   *  expression.
   *
   *  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a
   *  later exercise.
   */
  extension (t: Tree[Int]) def maximum: Int = t match {
    case Leaf(n)      => n
    case Branch(l, r) => l.maximum max r.maximum
  }

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(identity)(_ max _)

}

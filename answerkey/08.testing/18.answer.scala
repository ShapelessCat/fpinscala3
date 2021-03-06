// * `l.takeWhile(f) ++ l.dropWhile(f) == l`
// * We want to enforce that `takeWhile` returns the _longest_ prefix whose
//   elements satisfy the predicate. There are various ways to state this, but
//   the general idea is that the remaining list, if non-empty, should start
//   with an element that does _not_ satisfy the predicate.

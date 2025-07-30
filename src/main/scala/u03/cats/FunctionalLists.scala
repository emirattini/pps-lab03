package u03.cats

object FunctionalLists:

  type List[A, B] = (B, (A, B) => B) => B
  val Empty: [A, B] => () => List[A, B] =
    [A, B] => () => (empty: B, f: (A, B) => B) => empty

  val Cons: [A, B] => (A, List[A, B]) => List[A, B] =
    [A, B] => (head: A, tail: List[A, B]) =>
      (value: B, f: (A, B) => B) => f(head, tail(value, f))


@main def tryFunctionalLists(): Unit =
  import FunctionalLists.*

  val list: [B] => () => List[Int, B] =
    [B] => () => Cons(1, Cons(2, Cons(3, Empty())))
  println(list()("Empty", (x, y) => "Cons(" + x + "," + y + ")"))

  val sum = list()(0, (x, y) => x + y)
  println(sum)
  val product = list()(1, (x, y) => x * y)
  println(product)

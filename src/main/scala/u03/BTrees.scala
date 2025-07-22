package u03

object BTrees extends App:
  enum Tree[E]:
    case Leaf(value: E)
    case Node(left: Tree[E], right: Tree[E])

  object Tree:
    def fold[A, B](t: Tree[A])(mapper: A => B, combinator: (B, B) => B): B = t match
      case Leaf(a) => mapper(a)
      case Node(left, right) => combinator(fold(left)(mapper, combinator), fold(right)(mapper, combinator))

    def size[A](t: Tree[A]): Int =
      fold(t)(_ => 1, _ + _)

    def contains[A](t: Tree[A], elem: A): Boolean =
      fold(t)(_ == elem, _ || _)

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)), (t1, t2) => Node(t1, t2))

    def count[A](t: Tree[A], elem: A): Int =
      fold(t)(a => if a == elem then 1 else 0, _ + _)

  import Tree.*

@main def tryTrees(): Unit =
  import BTrees.Tree.*
  val tree = Node(Node(Leaf(1), Leaf(2)), Leaf(1))
  println(tree) // Branch(Branch(Leaf(1),Leaf(2)),Leaf(1))
  println(size(tree)) // ..,3
  println(contains(tree, 2))
  println(map(tree)(x => x * 8))
  println(count(tree, 1))

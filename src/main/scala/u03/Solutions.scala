package u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, filter, map}
import u03.Optionals.Optional
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty
import u03.Streams.Stream
import u03.Streams.Stream.{Cons, Empty, cons}

import java.util.function.BiFunction
import scala.annotation.tailrec

object Solutions:

  //Task 1 ------------------------------------------------------------------------------------------------------------

  @tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(h, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    case _ => Nil()

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
    case Cons(h, t) => Cons(h, concat(t, s2))
    case _ => s2

  def reverse[A](s: Sequence[A]): Sequence[A] =
    @tailrec
    def iter(s: Sequence[A], acc: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => iter(t, Cons(h, acc))
      case Nil() => acc

    iter(s, Nil())

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
    case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
    case _ => Nil()

  def min(s: Sequence[Int]): Optional[Int] =
    @tailrec
    def iter(s: Sequence[Int], min: Optional[Int]): Optional[Int] = s match
      case Cons(h, t) => min match
        case Empty() => iter(t, Just(h))
        case Just(x) if h < x => iter(t, Just(h))
        case _ => iter(t, min)
      case _ => min

    iter(s, Empty())

  def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, evenIndices(skip(t)(1)))
    case _ => s

  @tailrec
  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) if h == elem => true
    case Cons(_, t) => contains(t)(elem)
    case _ => false

  def distinct[A](s: Sequence[A]): Sequence[A] =
    def iter(s: Sequence[A], seen: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) if contains(seen)(h) => iter(t, seen)
      case Cons(h, t) => Cons(h, iter(t, Cons(h, seen)))
      case _ => s

    iter(s, Nil())

  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    def iter(s: Sequence[A], acc: Sequence[A]): Sequence[Sequence[A]] = (s, acc) match
      case (Cons(h1, t1), Cons(h2, _)) if h2 != h1 => Cons(acc, iter(t1, Cons(h1, Nil())))
      case (Cons(h, t), _) => iter(t, Cons(h, acc))
      case (Nil(), Nil()) => Nil()
      case (Nil(), _) => Cons(acc, Nil())

    iter(s, Nil())

  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    @tailrec
    def iter(s: Sequence[A], acc1: Sequence[A], acc2: Sequence[A]): (Sequence[A], Sequence[A]) = s match
      case Cons(h, t) if pred(h) => iter(t, concat(acc1, Cons(h, Nil())), acc2)
      case Cons(h, t) => iter(t, acc1, concat(acc2, Cons(h, Nil())))
      case Nil() => (acc1, acc2)

    iter(s, Nil(), Nil())

  //Task 2 ------------------------------------------------------------------------------------------------------------

  import u02.Modules.Person
  import u02.Modules.Person.*

  def isTeacher(p: Person): Boolean = p match
    case Teacher(_, _) => true
    case _ => false

  def getCourse(p: Person): String = p match
    case Teacher(_, course) => course
    case _ => ""

  def mapToCourses(persons: Sequence[Person]): Sequence[String] =
    map(filter(persons)(p => isTeacher(p)))(p => getCourse(p))

  def getCoursesNumber(persons: Sequence[Person]): Integer = {
    val courses = distinct(mapToCourses(persons))
    foldLeft(courses)(0)((a, _) => a + 1)
  }

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(start: B)(op: BiFunction[B, A, B]): B = s match
    case Cons(h, t) => foldLeft(t)(op(start, h))(op)
    case _ => start

  //Task 3 ------------------------------------------------------------------------------------------------------------

  import u03.Streams.Stream.Empty
  import u03.Streams.Stream.cons
  import u03.Streams.Stream.empty

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => empty()

    def fill[A](n: Int)(value: A): Stream[A] = n match
      case n if n > 0 => cons(value, fill(n - 1)(value))
      case _ => empty()

    def fibonacci(): Stream[Int] =
      def iter(i1: Int, i2: Int): Stream[Int] =
        cons(i1, iter(i2, i1 + i2))

      iter(0, 1)

    def interleave[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = stream1 match
      case Cons(h, t) => cons(h(), interleave(stream2, t()))
      case _ => stream2
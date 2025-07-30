package u03

import u03.extensionmethods.Sequences.Sequence
import Sequence.*
import u03.Streams.Stream.{cycle, cycle2, fillUnf}

import scala.annotation.tailrec

object Streams extends App:

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

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()

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

    def cycle[A](seq: Sequence[A]): Stream[A] =
      def iter(s: Sequence[A]): Stream[A] = s match
        case Sequence.Cons(head, tail) => cons(head, iter(tail))
        case Sequence.Nil() => iter(seq)
      iter(seq)

    def cycle2[A](s: Sequence[A]): Stream[A] = s match
      case Sequence.Cons(h, t) => cons(h, cycle(t append h))
      case Nil() => Empty()

    def unfold[A, B](seed: A)(stop: A => Boolean, f: A => B, next: A => A): Stream[B] =
      if stop(seed) then empty()
      else Cons(() => f(seed), () => unfold(next(seed))(stop, f, next))

    def fillUnf[A](n: Int)(value: A): Stream[A] =
      unfold(n)(_ == 0, _ => value, _ - 1)

  end Stream
end Streams

@main def tryStreams =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

  val repeat = Cons("a", Cons("b", Cons("c", Nil())))
  val innovativeStream = Stream.cycle(repeat)
  val value = Stream.toList(Stream.take(innovativeStream)(5))
  println(value)
  val innovativeStream2 = Stream.cycle2(repeat)
  val value2 = Stream.toList(Stream.take(innovativeStream2)(10))
  println(value2)

  println(Stream.toList(fillUnf(10)(3)))

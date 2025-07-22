package u03

import u02.Modules.Person
import u02.Modules.Person.Teacher
import u03.Optionals.Optional
import u03.Optionals.Optional.*

import java.util.function.BiFunction
import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:
//    def fold[A. B](l: Sequence[A])(start: B)(f: (A, B) => B): B = ???

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def unfold[A, B](seed: A)(stop: A => Boolean, f: A => B, next: A => A): Sequence[B] =
      if stop(seed) then Nil()
      else Cons(f(seed), unfold(next(seed))(stop, f, next))

    def fill[A](size: Int)(elem: => A): Sequence[A] =
      unfold(size)(stop = _ == 0, f =  _ => elem, next = _ - 1)

    def iterate[A](start: A, len: Int)(f: A => A): Sequence[A] =
      unfold((len, start))(stop = (len, _) => len == 0,
        f = (_, value) => value,
        next = (len, value) => (len - 1, f(value)))

    def unfoldOpt[A, B](in: A)(f: A => Option[(A, B)]): Sequence[B] = f(in) match
      case Some(next, value) => Cons(value, unfoldOpt(next)(f))
      case None => Nil()

    def fillOpt[A](size: Int)(elem: => A): Sequence[A] =
      unfoldOpt(size) {
          case size if size == 0 => None
          case size => Some(size - 1, elem)
      }

    def iterateOpt[A](start: A, len: Int)(f: A => A): Sequence[A] =
      unfoldOpt((len, start)) {
        case (len, value) if len > 0 => Some((len - 1, f(value)), value)
        case _ => None
      }

    def mapUnf[A, B](initial: Sequence[A])(f: A => B): Sequence[B] =
      unfoldOpt(initial) {
        case Cons(h, t) => Some(t, f(h))
        case Nil() => None
      }

  end Sequence
end Sequences

@main def trySequences(): Unit =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52

  println(fill(5)(3))
  println(iterate(99, 15)(x => x + 1))

  println(fillOpt(10)(6))
  val list = iterate(0, 5)(x => x + 1)
  println(list)
  println(mapUnf(list)(x => x * 2))

  println(iterateOpt(0, 10)(x => x + 3))
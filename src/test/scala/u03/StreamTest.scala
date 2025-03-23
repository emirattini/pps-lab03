package u03
import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import Stream.*
import u03.Sequences.{Sequence, *}
import Sequence.*

class StreamTest:

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    val expected = Cons("a", Cons("a", Cons("a", Nil())))
    val value = Stream.toList(Stream.fill(3)("a"))
    assertEquals(expected, value)

  @Test def testFibonacci(): Unit =
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    val fibonacci: Stream[Int] = Stream.fibonacci()
    val actual = Stream.toList(Stream.take(fibonacci)(5))
    assertEquals(expected, actual)

  @Test def testInterleave(): Unit =
    val stream1 = cons(1, cons(3, cons(5, empty())))
    val stream2 = cons(2, cons(4, cons(6, cons(8, cons(10, empty())))))
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil()))))))))
    assertEquals(expected, toList(interleave(stream1, stream2)))
end StreamTest

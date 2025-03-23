package u03

import org.junit.Test
import u03.Streams.Stream

class SolutionsTest {
  
  //Task 1 ------------------------------------------------------------------------------------------------------------
  
  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices() =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains() =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

  @Test def testGroup() =
    val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
    val grouped =
      Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
    assertEquals(grouped, group(sequence))
    assertEquals(Nil(), group(Nil()))

  @Test def testPartition() =
    val sequence = Cons(11, Cons(20, Cons(31, Nil())))
    val (even, odd) = partition(sequence)(x => x % 2 == 0)
    assertEquals(Cons(20, Nil()), even)
    assertEquals(Cons(11, Cons(31, Nil())), odd)

    val emptySequence = Nil()
    val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
    assertEquals(Nil(), evenEmpty)
    assertEquals(Nil(), oddEmpty)

  //Task 2 ------------------------------------------------------------------------------------------------------------
  
  import u02.Modules.*

  @Test def testMapToCourses() =
    val student = Student("Mario", 1)
    val course = "pps"
    val teacher = Teacher("Luigi", course)
    val persons = Cons(student, Cons(teacher, Nil()))
    assertEquals(Cons(course, Nil()), mapToCourses(persons))

  @Test def testGetCoursesNumber() =
    val student = Student("Mario", 1)
    val teacher1 = Teacher("Luigi", "pps")
    val teacher2 = Teacher("Sandro", "oop")
    val coursesNumber = 2
    val persons = Cons(teacher1, Cons(student, Cons(teacher2, Nil())))
    assertEquals(coursesNumber, getCoursesNumber(persons))

  @Test def testFoldLeft() =
    val numbers = Cons(3, Cons(4, Cons(5, Nil())))
    val sum = 3 + 4 + 5
    assertEquals(sum, foldLeft(numbers)(0)(_ + _))

  //Task 3 ------------------------------------------------------------------------------------------------------------
  
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
}

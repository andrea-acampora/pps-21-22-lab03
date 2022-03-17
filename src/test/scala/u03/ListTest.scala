package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Optionals.*
import u02.AlgebraicDataTypes.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1));

  @Test def testAppend(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, Cons(40, Nil())))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v+1, Nil())))

  @Test def testMapWithFlatmap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))

  @Test def testFilterWithFlatmap(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))

  @Test def testMax(): Unit =
    assertEquals(Option.Some(30), max(l))

  @Test def testCoursesByTeacher(): Unit =
    assertEquals(Cons("PPS", Cons("PCD", Nil())), getCoursesByTeacher(Cons(Person.Teacher("Viroli", "PPS"), Cons(Person.Teacher("Ricci", "PCD"), Nil()))))

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_-_))

  @Test def testFoldRight(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-8, foldRight(lst)(0)(_-_))

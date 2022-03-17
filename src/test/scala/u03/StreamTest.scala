package u03

import org.junit.*
import org.junit.Assert.*

import u03.Streams.*
import u03.Streams.Stream.*
import u03.Lists.List.Cons
import u03.Lists.List.Nil

class StreamTest :

  @Test def testDrop(): Unit =
    val stream: Stream[Int] = take(iterate(0)(_ + 1))(10)
    assertEquals( Stream.toList(cons(8, cons(9, empty()))), Stream.toList(drop(stream)(8)))

  @Test def testConstant(): Unit =
    assertEquals(Stream.toList(cons("x", cons("x", cons("x", empty())))), Stream.toList(take(constant("x"))(3)))

  @Test def testFib(): Unit =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Nil())))))), toList(take(fib())(6)))

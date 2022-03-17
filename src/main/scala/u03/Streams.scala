package u03

import u03.Lists.List.{Cons, Nil, drop}

import scala.annotation.tailrec

object Streams extends App:

  import Lists.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(_, tail), n) if n > 0 => drop(tail())(n-1)
      case (Cons(head, tail), n) => cons(head(), drop(tail())(n))
      case _ => Empty()

    def constant[A](value: A): Stream[A] = cons(value, constant(value))

    def fib(): Stream[Int] =
      def _fib(a: Int, b: Int): Stream[Int] = cons(b, _fib(b, a+b))
      cons(0, _fib(0,1))
      
  end Stream

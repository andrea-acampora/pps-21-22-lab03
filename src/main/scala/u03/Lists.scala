package u03

import u02.AlgebraicDataTypes.*
import u02.AlgebraicDataTypes.Person.*


import scala.annotation.tailrec
import u02.Optionals.Option.*
import u02.Optionals.*

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l1: List[A], n: Int): List[A] = l1 match
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case Cons(h, t) => Cons(h, t)
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, t), right) => Cons(h, append(t, right))
      case _ => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(x  => Cons(mapper(x), Nil()))

    def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)({ case x if pred(x) => Cons(x, Nil()); case _ => Nil() })

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h >= orElse(max(t), h) => Option.Some(h)
      case Cons(_, t) => max(t)
      case Nil() => Option.None()

    def getCoursesByTeacher(persons: List[Person]): List[String] =
      flatMap(persons)({ case Teacher(name, course) => Cons(course, Nil()); case _ => Nil() })

    def foldLeft[A, B](l: List[A])(value: B)(operator: (B, A) => B): B = l match
      case Cons(h, t) => operator(foldLeft(t)(value)(operator), h)
      case Nil() => value

    def foldRight[A, B](l: List[A])(value: B)(operator: (A, B) => B): B = l match
      case Cons(h, t) => operator(h, foldRight(t)(value)(operator))
      case Nil() => value

end Lists

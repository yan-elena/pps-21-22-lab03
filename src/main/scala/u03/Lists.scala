package u03

import u02.Modules.Person.{Teacher, name}
import u02.Modules.{Person, isStudent}
import scala.annotation.tailrec

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

    //exercise

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(h, t) if n > 0 => drop(t, n - 1)
    case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
    case Cons(h, t) => Cons(h, append(t, right))
    case _ => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil()

    //map function using flatMap
    def map2[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(h => Cons(mapper(h), Nil()))

    //filter function using flatMap
    def filter2[A](l1: List[A])(pred: A => Boolean): List[A] = flatMap(l1)(h => if (pred(h)) Cons(h, Nil()) else Nil())

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case Some(v) if h < v => Some(v)
        case _ => Some(h)
      case _ => None

    def getCourses(l: List[Person]): List[String] = flatMap(l) {
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    }

    @tailrec
    def foldLeft[A, B](l: List[A])(a: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(a, h))(f)
      case _ => a

    //foldRight using foldLeft with reverse function
    def foldRight[A, B](l: List[A])(a: B)(f: (A, B) => B): B = foldLeft(reverse(l))(a)((a, b) => f(b, a))

    //alternative foldRight function
    def foldRight2[A, B](l: List[A])(a: B)(f: (A, B) => B): B = l match
      case Cons(h, t) => f(h, foldRight2(t)(a)(f))
      case _ => a

    def reverse[A](l: List[A]): List[A] = l match
      case Cons(h, t) => append(reverse(t), Cons(h, Nil()))
      case _ => Nil()

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

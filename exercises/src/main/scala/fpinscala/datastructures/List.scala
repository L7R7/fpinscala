package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def sumTail(ints: List[Int]): Int = {
    @tailrec
    def go(acc: Int, is: List[Int]): Int = is match {
      case Nil => acc
      case Cons(x, xs) => go(acc + x, xs)
    }

    go(0, ints)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def productTail(ds: List[Double]): Double = {
    @tailrec
    def go(acc: Double, list: List[Double]): Double = list match {
      case Nil => acc
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => go(acc * x, xs)
    }

    go(1.0, ds)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val z: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def go(list: List[A], step: Int): List[A] = {
      if (step > 0) {
        list match {
          case Cons(_, tail) => go(tail, step - 1)
          case Nil => throw new NoSuchElementException("can't remove elements from empty list")
        }
      } else {
        list
      }
    }

    go(l, n)
  }

  @tailrec
  def drop2[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Cons(_, tail) => drop2(tail, n - 1)
      case Nil => Nil
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }

  @tailrec
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile2(tail)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, i) => i + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((i, _) => i + 1)

  def length3[A](l: List[A]): Int = foldRight(l, 0)((_, i) => i + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((tail, head) => Cons(head, tail))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  def append2[A](l: List[A], r: List[A]): List[A] = foldRight2(l, r)(Cons(_, _))

  def flattenLists[A](l: List[List[A]]): List[A] = foldRight2(l, Nil: List[A])(append2)

  def addOne(l: List[Int]): List[Int] = foldRight2(l, Nil: List[Int])((i: Int, tail: List[Int]) => Cons(i + 1, tail))

  def doubleToString(l: List[Double]): List[String] = foldRight2(l, Nil: List[String])((d: Double, tail: List[String]) => Cons(d.toString, tail))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil: List[B])((a: A, tail: List[B]) => Cons(f(a), tail))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((a: A, tail: List[A]) => if (f(a)) Cons(a, tail) else tail)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flattenLists(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = {
    @tailrec
    def go(acc: List[Int], as: List[Int], bs: List[Int]): List[Int] = as match {
      case Nil => acc
      case Cons(headA, tailA) => bs match {
        case Nil => acc
        case Cons(headB, tailB) => go(Cons(headA + headB, acc), tailA, tailB)
      }
    }

    reverse(go(Nil, as, bs))
  }

  def addPairwise2(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(hA, tA), Cons(hB, tB)) => Cons(hA + hB, addPairwise2(tA, tB))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(hA, tA), Cons(hB, tB)) => Cons(f(hA, hB), zipWith(tA, tB)(f))
  }

  def zipWith2[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(hA, tA), Cons(hB, tB)) => Cons(f(hA, hB), zipWith2(tA, tB)(f))
    case _ => Nil
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, Cons(_, _)) => false
    case (Cons(supH, supT), Cons(subH, subT)) if supH == subH => hasSubsequence(supT, subT)
    case (Cons(_, supT), _) => hasSubsequence(supT, sub)
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @tailrec
  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence2(t, sub)
  }

  def main(args: Array[String]): Unit = {
    val input = List(1, 2, 3, 4)
    println(foldRight(input, Nil: List[Int])(Cons(_, _)))
    println(foldRight2(input, Nil: List[Int])(Cons(_, _)))
    println(foldLeft(input, Nil: List[Int])((tail, head) => Cons(head, tail)))
    println(map(input)(_ * 2))
    println(reverse(input))
    println(length(input) == length2(input))
    println(length(input) == length3(input))
    println(length(Nil) == length2(Nil))

    val listOfLists = List(input, input, input)
    println(flattenLists(listOfLists))

    println(flatMap(List(1, 2, 3))(i => List(i, i)))

    println(addPairwise(List(1, 2, 3), List(4, 5, 6)))
    println(addPairwise2(List(1, 2, 3), List(4, 5, 6)))
    println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))
    println(zipWith2(List(1, 2, 3), List(4, 5, 6))(_ + _))

    println(zipWith(List(1, 2, 3), Nil: List[Int])(_ + _))
    println(zipWith2(List(1, 2, 3), Nil: List[Int])(_ + _))

    println(hasSubsequence(List(1, 2, 3), List(1)))
    println(hasSubsequence(List(1, 2, 3), List(2)))
    println(hasSubsequence(List(1, 2, 3), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3), List(1, 2, 3)))
    println(hasSubsequence(List(1, 2, 3), List(1, 2, 3, 4)))
  }
}

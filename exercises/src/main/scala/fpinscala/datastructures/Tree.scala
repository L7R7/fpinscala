package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    def go(as: Tree[A], acc: Int): Int = as match {
      case Leaf(_) => acc + 1
      case Branch(left, right) => acc + 1 + size(left) + size(right)
    }

    go(t, 0)
  }

  def size2[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size2(left) + size2(right)
  }

  def maximum(t: Tree[Int]): Int = {
    def go(as: Tree[Int], max: Int): Int = as match {
      case Leaf(value) => max.max(value)
      case Branch(left, right) => max.max(go(left, max).max(go(right, max)))
    }

    go(t, Int.MinValue)
  }

  def maximum2(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum2(left) max maximum2(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

  def fold2[A, B](f: A => B)(b: (B, B) => B)(t: Tree[A]): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) =>
      val function: Tree[A] => B = fold2(f)(b)
      b(function(l), function(r))
  }

  def sizeWithFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumWithFold(t: Tree[Int]): Int = fold(t)(i => i)(_ max _)

  def depthWithFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((b1, b2) => 1 + (b1 max b2))

  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    println(size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))))
    println(size2(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))))
    println(sizeWithFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))))
    println("---")
    println(maximum(Branch(Branch(Leaf(1), Branch(Leaf(222), Leaf(3))), Leaf(4))))
    println(maximum2(Branch(Branch(Leaf(1), Branch(Leaf(222), Leaf(3))), Leaf(4))))
    println(maximumWithFold(Branch(Branch(Leaf(1), Branch(Leaf(222), Leaf(3))), Leaf(4))))
    println("---")
    println(depth(Branch(Branch(Leaf(1), Branch(Leaf(222), Leaf(3))), Leaf(4))))
    println(depthWithFold(Branch(Branch(Leaf(1), Branch(Leaf(222), Leaf(3))), Leaf(4))))
    println("---")
    println(depth(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))
    println(depthWithFold(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))
    println("---")
    println(map(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)))(_ * 2))
    println(mapWithFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)))(_ * 2))
  }
}
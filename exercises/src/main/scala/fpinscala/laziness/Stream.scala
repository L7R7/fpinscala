package fpinscala.laziness

import fpinscala.laziness.Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTail: List[A] = {
    @tailrec
    def go(acc: List[A], s: Stream[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(h() :: acc, t())
    }

    go(Nil, this).reverse
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) empty
    else this match {
      case Empty => empty
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }

  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take2(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Empty => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, t) => if (p(a)) cons(a, t) else empty)

  @tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _ => false
  }

  def forAll2(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, t) => cons(f(a), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, t) => if (p(a)) cons(a, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, t) => cons(a, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, t) => f(a).append(t))

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(hA, tA), Cons(hB, tB)) if hA() == hB() => tA().startsWith(tB())
    case (_, Empty) => true
    case _ => false
  }

  def startsWith2[B](s: Stream[B]): Boolean = zipAll(this, s).takeWhile {
    case (Some(_), Some(_)) => true
    case _ => false
  } forAll {
    case (Some(a), Some(b)) => a == b
    case _ => false
  }

  def startsWith3[B](s: Stream[B]): Boolean = zipAll(this, s).takeWhile(_._2.isDefined) forAll {
    case (Some(a), Some(b)) => a == b
    case _ => false
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_, t) => Some((s, t()))
    case Empty => None
  } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail) // more efficient
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  val twos: Stream[Int] = unfold(2)(s => Some((s, s)))

  def constant3[A](a: A): Stream[A] = unfold(a)(s => Some((a, s)))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs2(): Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  // functions below could also be implemented as methods on Stream

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take[A](as: Stream[A], n: Int): Stream[A] = unfold((as, n))(f = s => s._1 match {
    case Cons(h, t) if s._2 > 0 => Some((h(), (t(), s._2 - 1)))
    case _ => None
  })

  def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] = unfold(as) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((as, bs)) {
    case (Cons(hA, tA), Cons(hB, tB)) => Some((f(hA(), hB()), (tA(), tB())))
    case _ => None
  }

  def zip[A, B](as: Stream[A], bs: Stream[B]): Stream[(A, B)] = zipWith(as, bs)((_, _))

  def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((as, bs)) {
    case (Cons(hA, tA), Cons(hB, tB)) => Some(((Some(hA()), Some(hB())), (tA(), tB())))
    case (Cons(hA, tA), _) => Some(((Some(hA()), None), (tA(), empty)))
    case (_, Cons(hB, tB)) => Some(((None, Some(hB())), (empty, tB())))
    case _ => None
  }

  def main(args: Array[String]): Unit = {
    println(ones.take(10).toListTail)
    println(twos.take(10).toListTail)

    println(from(1).take(10).toListTail)
    println(from2(1).take(10).toListTail)

    println(constant2(1).take(10).toListTail)
    println(constant3(1).take(10).toListTail)

    println(fibs().take(10).toListTail)
    println(fibs2().take(10).toListTail)

    println(map(ones.take(10))(_ * 2).toListTail)

    println(take(ones, 4).toListTail)

    println(takeWhile(fibs2())(_ < 20).toListTail)

    println(zipWith(ones.take(4), ones)(_ + _).toListTail)

    println(zipAll(ones.take(4), ones.take(6)).toListTail)
    println(zipAll(ones.take(6), ones.take(4)).toListTail)

    println(Stream(1, 2, 3) startsWith Stream(1, 2))
    println(Stream(1, 2, 3) startsWith2 Stream(1, 2))
    println(Stream(1, 2, 3) startsWith3 Stream(1, 2))
    println(Stream(1, 2, 3) startsWith Stream(2, 3))
    println(Stream(1, 2, 3) startsWith2 Stream(2, 3))
    println(Stream(1, 2, 3) startsWith3 Stream(2, 3))

    println(Stream(1, 2, 3).tails.map(_.toListTail).toListTail)

    println(Stream(1, 2, 3).scanRight(0)(_ + _).toListTail)
  }
}
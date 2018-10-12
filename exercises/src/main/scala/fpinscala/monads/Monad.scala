package fpinscala
package monads

import fpinscala.parallelism.Par._
import fpinscala.parallelism._
import fpinscala.parsing._
import fpinscala.state._
import fpinscala.testing._

import scala.language.{higherKinds, reflectiveCalls}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  // recursive version
  def _replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(List[A]())
    else map2(ma, _replicateM(n - 1, ma))(_ :: _)

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  //  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
  //    map2(traverse(ms)(unit), traverse(ms)(f))((as, bs) => as.zip(bs).filter(_._2).map(_._1))

  // from the solution
  def _filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) _filterM(t)(f)
        else map(_filterM(t)(f))(h :: _))
    }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(v => v)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  def _compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)

    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }
}

/*
11.5
List:
* gets a List in, turns it into a List of lists
* there are n lists, all lists have the same size
Option:
* gets in an Option (Some or None)
* if None, List[None] is returned (?)
* if Some, a list with n times the Some(_) is returned
 */

/* 11.9
flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
compose((_: Unit) => compose((_: Unit) => x, f)(()), g)(()) == compose((_: Unit) => x, a => compose((_: Unit) => f(a), g)(()))(())

compose(compose(f, g), h) == compose(f, compose(g, h))
rewrite outer `compose` calls
  a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))

rewrite inner `compose` calls
  a => flatMap((b => flatMap(f(b))(g))(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

simplify left side
  a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

substitute `f(a)` with `x`
  flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))
 */

/* 11.10
compose(f, unit) == f
compose(unit, f) == f

flatMap(x)(unit) == x
flatMap(unit(y))(f) == f(y)

compose(f, unit)(v) == f(v)           // for all functions f and values v
(a => flatMap(f(a))(unit))(v) == f(v) // Expand `compose`
flatMap(f(v))(unit) == f(v)           // Simplify function application
flatMap(x)(unit) == x                 // Abstract out `f(v)`

compose(unit, f)(x) == f(x)
flatMap(unit(x))(f) == f(x) // Expand `compose`
 */

/* 11.11
identity laws for Option:

flatMap(x)(unit) == x
flatMap(unit(y))(f) == f(y)

left identity:
flatMap(None)(Some(_)) == None
None == None

flatMap(Some(v)(Some(_)) = Some(v)
Some(v) == Some(v)

right identity:
flatMap(Some(None))(f) == f(None)
f(None) == f(None)

flatMap(Some(Some(v)))(f) == f(Some(v))
f(Some(v)) == f(Some(v))
 */
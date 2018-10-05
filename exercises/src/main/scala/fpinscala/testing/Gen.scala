package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness._
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.state.{RNG, State}
import fpinscala.testing.Prop._

import scala.annotation.tailrec

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/*
8.1: properties for sum: List[Int] => Int
* list.sum == list.reverse.sum
* list.map(_ * x).sum == list.sum * x
* list.map(f).sum == f(list.sum) for all f: Int => Int
* list.sum == list.length * x if all elements in list == x
* list.sum == 0 if list has no elements
* List(a,b,c).sum == List(b,a,c).sum == List(c,a,b).sum == List(a,c,b).sum == .... (addition is commutative)
* List(1,2,3,....,n).sum == n*(n+1) / 2
 */

/*
8.2: properties for max: List[Int] => Int
* list.max == list.reverse.max
* list.max == ? if list is empty
* list.map(f).max == f(list.max) for all f: Int => Int
* the order of the elements in the list has no effect on the result of the max function
* list.max == x if the list only contains x
* list.forAll(x => x >= list.max)
* list.contains(list.max)
 */

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      (this.run(max, n, rng), p.run(max, n, rng)) match {
        case (Passed, Passed) => Passed
        case (Proved, Proved) => Proved
        case (Falsified(fc1, s1), Falsified(fc2, s2)) => Falsified(s"both failed with reasons:\n$fc1\n$fc2", s1 + s2)
        case (Falsified(f, s), _) => Falsified(f, s)
        case (_, Falsified(f, s)) => Falsified(f, s)
        case (Passed, Proved) => Passed
        case (Proved, Passed) => Passed
      }
    }
  }

  // make use of short-circuiting: don't run the second Prop if the first one failed
  def &&&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case f@Falsified(_, _) => f
        case _ => p.run(max, n, rng)
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      (this.run(max, n, rng), p.run(max, n, rng)) match {
        case (Falsified(fc1, s1), Falsified(fc2, s2)) => Falsified(s"both failed with reasons:\n$fc1\n$fc2", s1 + s2)
        case _ => Passed
      }
    }
  }

  def |||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case s => s
      }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(s"$msg\n$e", c)
        case x => x
      }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      Stream.zip(randomStream(as)(rng), Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       | ${e.getStackTrace.mkString("\n")}""".stripMargin

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max, _, rng) => p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  import Gen.{**, choose, unit, weighted}

  // todo shouldn't 5 be used as second argument for choose?
  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar3(Gen.unit(()))(_ => p)

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println("+ OK, proved property.")
    }

  def check1(p: => Boolean): Prop = {
    lazy val result = p
    forAll(Gen.unit(()))(_ => result)
  }

  def check2(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Passed else Falsified(" ()", 0) }

  def check(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Proved else Falsified(" ()", 0) }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def mapWithFlatMap[B](f: A => B): Gen[B] = flatMap(a => Gen(State.unit(f(a))))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def listOf1(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n max 1, this))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(1)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val w = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < w) g1._1.sample else g2._1.sample))
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def main(args: Array[String]): Unit = {
    //    println(unit(4).sample.run(RNG.Simple(11))._1)
    //    println(choose(0, 12).sample.run(RNG.Simple(12)))
    //    println(boolean.sample.run(RNG.Simple(1)))

    //    checkMax()
    //    checkSorted()
    //    checkParallel()
    checkTakeWhile()
  }

  def checkMax(): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  def checkSorted(): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = forAll(SGen.listOf(smallInt)) { ns =>
      @tailrec
      def isSorted(xs: List[TestCases]): Boolean = xs match {
        case Nil => true
        case _ :: Nil => true
        case head :: tl => head <= tl.min && isSorted(tl)
      }

      val sorted = ns.sorted
      isSorted(sorted) && sorted.forall(ns.contains(_)) && ns.forall(sorted.contains(_))
    }
    Prop.run(sortedProp)
  }

  def checkParallel(): Unit = {
    val ES: ExecutorService = Executors.newCachedThreadPool
    val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
    Prop.run(p1)

    val p2 = Prop.check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }
    Prop.run(p2)


    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

    val p3 = check {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    }
    Prop.run(p3)

    val p4 = checkPar {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }
    Prop.run(p4)

    val pint = Gen.choose(0, 10) map (Par.unit(_))
    val p5 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    Prop.run(p5)

    val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(_ + _)
        }))
    val p6 = forAllPar3(pint2)(n => equal(Par.map(n)(y => y), n))
    Prop.run(p6)

    val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
    Prop.run(forkProp)
  }

  def checkTakeWhile(): Unit = {
    val isEven = (i: Int) => i % 2 == 0
    val takeWhileProp = Prop.forAll(Gen.listOf(Gen.choose(Int.MinValue, Int.MaxValue)))(ns => ns.takeWhile(isEven).forall(isEven))
    Prop.run(takeWhileProp)

    def genStringintFn(g: Gen[Int]): Gen[String => Int] =
      g map (i => s => i)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val s: Int => Gen[B] = n => {
      forSize(n).flatMap({
        f(_).forSize(n)
      })
    }
    SGen(s)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOf1(n, g))
}

/*
8.18
* result of takeWhile is smaller or equal in size than the input list
* the first element in the input list that comes after the result of takeWhile doesn't match the predicate
* the result of takeWhile (with length of n) and the first n elements of the input are the same => make sure that the longest prefix is returned
* takeWhile ++ dropWhile = input list
 */